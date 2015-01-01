
{-# LANGUAGE OverloadedStrings #-}

module Bitcoin.Base58 where


import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- import qualified Data.ByteString.Unsafe as B

import Data.Word
import Data.Bits
import Data.Maybe

-- $setup
-- >>>
-- >>> import Control.Applicative
-- >>>
-- >>> import qualified Data.ByteString as B
-- >>> import qualified Data.ByteString.UTF8 as B
-- >>> import qualified Data.ByteString.Base16 as B16
-- >>>
-- >>> import Bitcoin.Util
-- >>> 
-- >>> import Test.QuickCheck
-- >>>
-- >>> instance Arbitrary B.ByteString where arbitrary = B.pack <$> arbitrary
-- >>> instance CoArbitrary B.ByteString where coarbitrary = coarbitrary . B.unpack



-- lookup tables

codeString :: ByteString
codeString = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

codeString' :: ByteString
codeString' = B.unfoldr go [0..123]
  where go (x:xs) = Just (fromIntegral $ fromMaybe 0 (B.findIndex (== x) codeString), xs)
        go []     = Nothing

forwardCodeString :: Integral a => a -> Word8
forwardCodeString i = codeString `B.index` fromIntegral i

reverseCodeString :: Integral a => Word8 -> Maybe a
reverseCodeString i | i < B.head "1" = Nothing 
                    | i > B.head "z" = Nothing 
                    | otherwise = Just . fromIntegral $ codeString' `B.index` fromIntegral i 

-- | encode a bytestring in base58
-- >>> import qualified Data.ByteString.Base16 as B16
--
-- >>> let test = hex2bs "005cc87f4a3fdfe3a2346b6953267ca867282630d3f9b78e64" 
-- >>> encode test 
-- "19TbMSWwHvnxAKy12iNm3KdbGfzfaMFViT"

encode :: ByteString -> ByteString
encode d  = B.reverse $ payload `B.append` zeros 
  where 
        -- split zeros from front of string
        (ds,zs) = B.spanEnd (== 0) (B.reverse d)
        
        -- extra zeros
        zeros = B.map (const (B.head "1")) zs

        -- actual encoding
        l = B.length d
        payload = fst $ B.unfoldrN (l*2) go (roll ds) 
                

        go x = if x > 0 
          then Just (forwardCodeString r, x') 
          else Nothing
          where (x',r) = x `divMod` 58


        

-- | encode a bytestring in base58
--
-- >>> let test = B.fromString "19TbMSWwHvnxAKy12iNm3KdbGfzfaMFViT"
-- >>> B16.encode . decode $ test 
-- "005cc87f4a3fdfe3a2346b6953267ca867282630d3f9b78e64"
--
-- prop> ((decode . encode $ s) == s)
decode :: ByteString -> ByteString
decode d = B.reverse $ payload `B.append` ones 
  where unsafeIndex = fromJust . reverseCodeString
        (ds,os)     = B.spanEnd (== B.head "1") (B.reverse d)

        payload     = unroll . B.foldr' go 0 $ ds
        go w i      = unsafeIndex w + i * 58

        -- extra ones
        ones        = B.map (const 0) os

-- fold and unfold an integer to and from a list of its bytes
-- from http://hackage.haskell.org/packages/archive/cereal/0.3.5.2/doc/html/src/Data-Serialize.html
unroll :: Integer -> ByteString 
unroll = B.unfoldr step
  where step 0 = Nothing
        step i = Just (fromIntegral i, i `shiftR` 8)

roll :: ByteString -> Integer
roll = B.foldr unstep 0
  where unstep b a = a `shiftL` 8 .|. fromIntegral b

