  
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bitcoin.Protocol where

import System.Random

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Serialize (Serialize, Get
                      ,put, putWord8, putWord16le, putWord32le, putWord64le
                      ,get, getWord8, getWord16le, getWord32le, getWord64le
                      ,getByteString, putByteString
                      ) 
import qualified Data.Serialize as S

import Data.Word 
import Data.Int
import Data.Bits
import Data.String
import Data.Monoid

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.EnumSet as Set

import Data.Time.Clock.POSIX


import Network
import Network.Socket 

import Control.Applicative

import Bitcoin.Util
import Bitcoin.Crypto

import GHC.Generics

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>>
-- >>> import Bitcoin.Util
-- >>> import qualified Data.Serialize as S
-- >>> import qualified Data.ByteString.Base16 as B16
--
-- >>> import qualified Data.EnumSet as Set
-- >>> import Data.List 
--
-- >>> import qualified Data.Text as T
-- >>> import qualified Data.Text.Encoding as T
--
-- >>> instance (Arbitrary w, Bits w) => Arbitrary (Set.T w a) where arbitrary = Set.Cons <$> arbitrary
--
-- >>> let arbIP4 = SockAddrInet <$> (PortNum <$> arbitrary) <*> arbitrary
-- >>> instance Arbitrary SockAddr where arbitrary = do a <- arbIP4; elements [a]
-- >>> instance Arbitrary Services where arbitrary = Services . nub <$> listOf (elements [NodeNetwork])
-- >>> instance Arbitrary NetworkAddress where arbitrary = NetworkAddress <$> arbitrary <*> arbitrary
-- >>> instance Arbitrary VarInt where arbitrary = VarInt <$> choose (0, 2^64-1) 
--
-- >>> instance Arbitrary T.Text where arbitrary = T.pack <$> arbitrary
-- >>> instance Arbitrary VarString where arbitrary = VarString <$> arbitrary 
-- >>> instance Arbitrary Version where arbitrary = Version <$> arbitrary 
-- >>> instance Arbitrary Timestamp where arbitrary = Timestamp . fromIntegral <$> (arbitrary :: Gen Word)
-- >>> instance Arbitrary MsgVersion where arbitrary = MsgVersion <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 



---------------------------------

newtype Word32LE = Word32LE Word32
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

instance Serialize Word32LE where
  put = putWord32le . fromIntegral
  get = fromIntegral <$> getWord32le

---------------------------------


btcDefaultPort :: PortID 
btcDefaultPort = PortNumber 8333

---------------------------------

btcMagic :: Word32LE
btcMagic = 0xD9B4BEF9

---------------------------------

class Serialize a => BtcMessage a where
  bmEncode :: a -> ByteString
  bmEncode = S.encode
  bmDecode :: ByteString -> Either String a
  bmDecode = S.decode
  bmCommand :: a -> Command 

---------------------------------
 
-- | all types of network services
-- | currently only NodeNetwork
-- 
-- enumset ist used to derive the Word64 
-- used in some messages, so order 
-- is important
data Service = NodeNetwork 
  deriving (Eq, Show, Ord, Enum)

-- | set of services
--newtype Services = Services {unServices :: Set.T Word64 Service}
newtype Services = Services {unServices :: [Service]} 
  deriving (Show, Eq)

-- | Serialize services to bit field
instance Serialize Services where
    get = Services . Set.toEnums . Set.Cons <$> getWord64le 
    put = putWord64le . Set.decons . Set.fromEnums . unServices

-- no idea why this is not included in enumset
instance (Bits w, Show a, Enum a) => Show (Set.T w a) where
  show e = show $ Set.toEnums e


---------------------------------

-- varints

newtype VarInt = VarInt {unVarInt :: Integer}
  deriving (Show,Eq,Ord,Num,Integral,Real,Enum)

instance Serialize VarInt where
  put i | i < 0           = error "VarInt doesn't support values below zero"
        | i <  0xfd       = putWord8 (fromIntegral i)
        | i <= 0xffff     = putWord8 0xfd >> putWord16le (fromIntegral i)
        | i <= 0xffffffff = putWord8 0xfe >> putWord32le (fromIntegral i)
        | i <= maxWord64  = putWord8 0xff >> putWord64le (fromIntegral i)
        | otherwise       = error "Integer too big for VarInt, sorry that"
    where maxWord64 = fromIntegral (maxBound :: Word64)

  get = getWord8 >>= \b -> 
    if b < 0xfd 
      then return (fromIntegral b)
      else case b of 
        0xfd -> fromIntegral <$> getWord16le
        0xfe -> fromIntegral <$> getWord32le
        0xff -> fromIntegral <$> getWord64le

-- var strings

newtype VarString = VarString {unVarString :: T.Text} 
  deriving (Show,Eq,Ord,IsString)

instance Serialize VarString where
  put (VarString t) = do
    let b = T.encodeUtf8 t
    put (fromIntegral . B.length $ b :: VarInt)
    putByteString (T.encodeUtf8 t)

  get = do
    l <- fromIntegral <$> (get :: Get VarInt)
    VarString . T.decodeUtf8 <$> getByteString l


---------------------------------
  
newtype Timestamp = Timestamp POSIXTime
  deriving (Enum, Eq, Fractional, Num, Ord, Real, RealFrac, Show)

unTimestamp :: Timestamp -> POSIXTime
unTimestamp (Timestamp p) = p

instance Serialize Timestamp where
  get = fromIntegral <$> getWord64le
  put = putWord64le . floor 

---------------------------------

newtype Version = Version {unVersion :: Int32}
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show)

instance Serialize Version where
  get = fromIntegral <$> getWord32le
  put = putWord32le . fromIntegral 

---------------------------------


instance Serialize PortNumber where
  get = PortNum <$> get
  put (PortNum p) = put p

instance Serialize SockAddr where
  get = do
    (a,b,c,d,p) <- (,,,,) <$> get <*> get <*> get <*> get <*> get 

    return $ if c == 0xffff
                     then SockAddrInet p d
                     else SockAddrInet6 p 0 (a,b,c,d) 0xe

  put (SockAddrInet (PortNum p) d) = do
        put (0      :: Word32)
        put (0      :: Word32)
        put (0xffff :: Word32)
        put d
        put p
  put (SockAddrInet6 (PortNum p) _ (a,b,c,d) _) = do
        put a 
        put b
        put c 
        put d
        put p


-- |
--
-- >>> let h = hex2bs "01 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 00 00 FF FF 0A 00 00 01  20 8D"
-- >>> let (Right n) = S.decode h :: Either String NetworkAddress 
-- >>> n
-- NetworkAddress {naServices = Services {unServices = [NodeNetwork]}, naAddress = 1.0.0.10:36128}
--
-- >>> B16.encode $ S.encode n
-- "010000000000000000000000000000000000ffff0a000001208d"
--

data NetworkAddress = NetworkAddress {
  naServices :: Services,
  naAddress :: SockAddr
} deriving (Eq, Show, Generic)

instance Serialize NetworkAddress where

---------------------------------

newtype Command = Command T.Text
  deriving (Eq, Ord, Read, Show, IsString)

unCommand :: Command -> T.Text
unCommand (Command t) = t

instance Serialize Command where
  put (Command t) = putByteString $ c <> zs
    where zs = B.replicate (l - B.length c) 0 
          c  = B.take l (T.encodeUtf8 t)
          l  = 12
  get = Command . T.filter (/= '\0') . T.decodeUtf8 <$> getByteString 12

---------------------------------


data MsgHeader = MsgHeader {
  mhMagic :: Word32LE,
  mhCommand :: Command,
  mhLength :: Word32LE,
  mhChecksum :: Checksum
} deriving (Eq, Show, Generic)

instance Serialize MsgHeader where


mkMessage m = header <> payload 
  where magic    = btcMagic 
        command  = bmCommand m
        payload  = bmEncode m
        len      = fromIntegral $ B.length payload
        checksum = btcChecksum payload 
        header   = S.encode $ MsgHeader magic command len checksum

---------------------------------

-- | 
--
-- test data from the specification
--
-- >>> let a = S.decode testMsgVersion :: Either String MsgVersion
-- >>> print a
-- Right (MsgVersion {mvVersion = Version {unVersion = 60002}, mvServices = Services {unServices = [NodeNetwork]}, mvTimestamp = Timestamp {unTimestamp = 1355854353s}, mvAddrRecv = NetworkAddress {naServices = Services {unServices = [NodeNetwork]}, naAddress = 0.0.0.0:0}, mvAddrFrom = NetworkAddress {naServices = Services {unServices = [NodeNetwork]}, naAddress = 0.0.0.0:0}, mvNonce = 4264543111543658341, mvUserAgent = VarString {unVarString = "/Satoshi:0.7.2/"}, mvStartHeight = 3225289472, mvRelay = True})
--
-- >>> let (Right b) = a
-- >>> B16.encode $ S.encode b 
-- "62ea0000010000000000000011b2d05000000000010000000000000000000000000000000000ffff000000000000010000000000000000000000000000000000ffff0000000000003b2eb35d8ce617650f2f5361746f7368693a302e372e322fc03e030001"
data MsgVersion = MsgVersion {
  mvVersion     :: Version,
  mvServices    :: Services,
  mvTimestamp   :: Timestamp,
  mvAddrRecv    :: NetworkAddress,
  mvAddrFrom    :: NetworkAddress,
  mvNonce       :: Word64,
  mvUserAgent   :: VarString,
  mvStartHeight :: Word32LE
} deriving (Show, Eq, Generic)

instance Serialize MsgVersion where

instance BtcMessage MsgVersion where
  bmCommand = const "version"

defaultMsgVersion timestamp nonce startHeight = MsgVersion {
  mvVersion = Version 60002, 
  mvServices = Services [],
  mvTimestamp = timestamp, 
  mvAddrRecv = NetworkAddress {
    naServices = Services [], 
    naAddress = SockAddrInet 0 0 
  },
  mvAddrFrom = NetworkAddress {
    naServices = Services [],
    naAddress = SockAddrInet 0 0 
  }, 
  mvNonce = nonce, 
  mvUserAgent = "", 
  mvStartHeight = startHeight 
}

mkMsgVersion = do
  t <- Timestamp <$> getPOSIXTime
  r <- randomIO
  
  return $ defaultMsgVersion t r 0



---------------------------------
-- test data


testMsgVersion :: ByteString
testMsgVersion = hex2bs $ "62 ea 00 00"                                     -- version
                       ++ "01 00 00 00 00 00 00 00"                         -- services 
                       ++ "11 b2 d0 50 00 00 00 00"                         -- timestamp
                       ++ "010000000000000000000000000000000000ffff000000000000" -- recv addr
                       ++ "000000000000000000000000000000000000ffff000000000000" -- send addr
                       ++ "3b 2e b3 5d 8c e6 17 65"                         -- nonce
                       ++ "0f 2f 53 61 74 6f 73 68 69 3a 30 2e 37 2e 32 2f" -- user agent
                       ++ "c0 3e 03 00"                                     -- last block
                       -- ++ "01"                                              -- relay

testResponse :: ByteString
testResponse = hex2bs $ 

                 "f9 be b4 d9  76 65 72 73  69 6f 6e 00  00 00 00 00" 
              ++ "64 00 00 00  5b 6f 5f 76  71 11 01 00  01 00 00 00" 
              ++ "00 00 00 00  8b 1f 71 51  00 00 00 00  01 00 00 00" 
              ++ "00 00 00 00  00 00 00 00  00 00 00 00  00 00 ff ff" 
              ++ "81 46 d9 13  e0 e2 01 00  00 00 00 00  00 00 00 00" 
              ++ "00 00 00 00  00 00 00 00  ff ff 47 ff  f3 eb 20 8d" 
              ++ "04 d2 e0 93  45 00 b2 43  0f 2f 53 61  74 6f 73 68" 
              ++ "69 3a 30 2e  38 2e 31 2f  9e 8a 03 00  f9 be b4 d9" 
              ++ "76 65 72 61  63 6b 00 00  00 00 00 00  00 00 00 00" 
              ++ "5d f6 e0 e2  f9 be b4 d9  69 6e 76 00  00 00 00 00" 
              ++ "00 00 00 00  25 00 00 00  a9 21 22 95  01 01 00 00" 
              ++ "00 e1 51 9c  e5 79 32 c6  46 bc dd 39  86 cf c8 3a" 
              ++ "f2 7e 5b ec  eb 29 b5 50  bf 5d d5 26  37 fd 5b c4" 
              ++ "8e f9 be b4  d9 69 6e 76  00 00 00 00  00 00 00 00" 
              ++ "00 25 00 00  00 49 7c 0c  8c 01 01 00  00 00 f5 dc" 
              ++ "b4 dc 61 a7  96 33 6f cf  e0 63 08 64  f5 08 d3 d9" 
              ++ "62 e4 5b 60  14 5f 1d 58  9e 05 2f 36  c2 17 f9 be" 
              ++ "b4 d9 69 6e  76 00 00 00  00 00 00 00  00 00 25 00" 
              ++ "00 00 32 76  72 b0 01 01  00 00 00 85  47 47 e0 c6" 
              ++ "d0 a1 b0 eb  4d 9f 04 c6  b7 b0 40 55  c4 cc ac e8" 
              ++ "fb 05 67 a1  76 fb 6c a8  84 e4 51 f9  be b4 d9 69" 
              ++ "6e 76 00 00  00 00 00 00  00 00 00 25  00 00 00 da" 
              ++ "fb 89 1e 01  01 00 00 00  0d 8e 72 02  89 eb 56 11" 
              ++ "26 2c 35 a6  4e c6 3a 40  78 95 bf 64  fe f3 9e 48" 
              ++ "83 a2 49 78  ae fd 28 d1  f9 be b4 d9  69 6e 76 00" 
              ++ "00 00 00 00  00 00 00 00  25 00 00 00  4a 3b 53 86" 
              ++ "01 01 00 00  00 da 02 11  d1 19 48 f3  92 95 91 51" 
              ++ "35 c9 66 ad  59 18 79 f2  8e ac 84 20  14 7f 70 fc" 
              ++ "e2 28 45 83  b2"
