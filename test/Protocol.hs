
module Protocol where

import Bitcoin.Protocol


import qualified Data.Serialize as S

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Applicative

import Test.QuickCheck

import Network.Socket

import Data.List 
import Data.Word


--instance (Arbitrary w, Bits w) => Arbitrary (Set.T w a) where arbitrary = Set.Cons <$> arbitrary


instance Arbitrary SockAddr where 
  arbitrary = do 
    a <- SockAddrInet <$> (PortNum <$> arbitrary) <*> arbitrary
    elements [a]

instance Arbitrary Services where 
  arbitrary = Services . nub <$> listOf (elements [NodeNetwork])

instance Arbitrary NetworkAddress where 
  arbitrary = NetworkAddress <$> arbitrary <*> arbitrary

instance Arbitrary VarInt where 
  arbitrary = VarInt <$> choose (0, fromIntegral (maxBound :: Word64)) 

instance Arbitrary T.Text where 
  arbitrary = T.pack <$> arbitrary
instance Arbitrary VarString where 
  arbitrary = VarString <$> arbitrary 
instance Arbitrary Version where 
  arbitrary = Version <$> arbitrary 
instance Arbitrary Timestamp where 
  arbitrary = Timestamp . fromIntegral <$> (arbitrary :: Gen Word)

instance Arbitrary MsgVersion where 
  arbitrary = MsgVersion <$> arbitrary 
                         <*> arbitrary 
                         <*> arbitrary 
                         <*> arbitrary 
                         <*> arbitrary 
                         <*> arbitrary 
                         <*> arbitrary 
                         <*> arbitrary 
                         <*> arbitrary 



testCoding x = (S.decode . S.encode) x == Right x
