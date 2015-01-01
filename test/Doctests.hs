module Main where

import Test.DocTest


main :: IO ()
main = doctest ["-isrc"
               ,"-XOverloadedStrings"
               ,"Bitcoin"
               ,"Bitcoin.Base58"
               ,"Bitcoin.Util"
               ]
