

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bitcoin.Crypto where

import Control.Applicative

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B (fromString, toString)

import Data.Word

import qualified Data.Serialize as S

import qualified Data.ByteString.Base16 as B16

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.RIPEMD160 as RIPEMD160


import qualified Crypto.Cipher.ECDSA as ECDSA

hello :: ByteString
hello = "hello"

------------------------------

newtype Hash = Hash {unHash :: ByteString}
  deriving (Eq)

instance Show Hash where
  show = B.toString . B16.encode . unHash

-- |
-- >>> btcHash hello
-- 9595c9df90075148eb06860365df33584b75bff782a510c6cd4883a419833d50
btcHash :: ByteString -> Hash 
btcHash = Hash . SHA256.hash . SHA256.hash

------------------------------

newtype Address = Address {unAddress :: ByteString}

instance Show Address where
  show = B.toString . B16.encode . unAddress


-- |
-- >>> btcAddress $ B.fromString "hello"
-- b6a9c8c230722b7c748331a8b450f05566dc7d0f
btcAddress :: ByteString -> Address
btcAddress = Address . RIPEMD160.hash . SHA256.hash

------------------------------

newtype Checksum = Checksum ByteString
  deriving (Eq, Ord, Read, Show)

unChecksum :: Checksum -> ByteString
unChecksum (Checksum b) = b

instance S.Serialize Checksum where
  put = S.putByteString . unChecksum
  get = Checksum <$> S.getByteString 4


btcChecksum :: ByteString -> Checksum 
btcChecksum b = Checksum . B.take 4 . unHash . btcHash $ b

------------------------------



secp256k1Cofactor = 0x01

secp256k1CurveParameters :: ECDSA.CurveParameters
secp256k1CurveParameters = ECDSA.CurveParameters 512 
                            secp256k1Curve 
                            secp256k1BasePoint
                            secp256k1Order

secp256k1Curve = ECDSA.Curve a b p
  where a = 0x0000000000000000000000000000000000000000000000000000000000000000
        b = 0x0000000000000000000000000000000000000000000000000000000000000007
        p = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F

secp256k1BasePoint = ECDSA.Point gx gy
  where gx = 0x79BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F81798
        gy = 0x483ADA7726A3C4655DA4FBFC0E1108A8FD17B448A68554199C47D08FFB10D4B8

secp256k1Order = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141


btcSign = ECDSA.sign secp256k1CurveParameters id

btcVerify = ECDSA.verify secp256k1CurveParameters id

-- | test signing and verification
-- >>> import Data.Maybe
-- >>> import System.Random
-- >>> import Crypto.Cipher.ECDSA.Math
-- >>> import qualified Data.ByteString.Lazy as LB
-- >>> import qualified Data.ByteString.Lazy.UTF8 as LB
-- 
-- >>> private <- randomRIO (1, secp256k1Order - 1)
-- >>> let public = fromJust $ pointMul secp256k1Curve private secp256k1BasePoint
-- 
-- >>> let msg  = (LB.fromString) "Hallo Welt!"
-- >>> let msg' = (LB.fromString) "Hallo Welt."
-- >>> sig <- btcSign private msg
-- 
-- >>> btcVerify public sig msg
-- True
-- >>> btcVerify public sig msg'
-- False




------------------------------

newtype BTCAddress = BTCAddress ByteString
  deriving (Show, Eq)




------------------------------
main = undefined
