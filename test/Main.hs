
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Bitcoin.Protocol
import Protocol

main = defaultMain tests

tests = [testGroup "test en- and decoding" 
          [testProperty "VarInt"    (testCoding :: VarInt -> Bool)
          ,testProperty "VarString" (testCoding :: VarString -> Bool)
          ,testProperty "Services"  (testCoding :: Services -> Bool)
          ,testProperty "NetworkAddress" (testCoding :: NetworkAddress -> Bool)
          ,testProperty "MsgVersion" (testCoding :: MsgVersion -> Bool)
          ]
        ]
