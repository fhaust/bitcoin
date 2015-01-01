
module Bitcoin.Util where


import Hexdump

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8  as B

import qualified Data.ByteString.Base16 as B16

import Data.Char (isSpace, toLower)

hexdump :: B.ByteString -> IO ()
hexdump = putStr . prettyHex 

-- |
-- >>> B16.encode $ hex2bs "01 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00 00 00 FF FF 0A 00 00 01  20 8D"
-- "010000000000000000000000000000000000ffff0a000001208d"
hex2bs :: String -> B.ByteString
hex2bs = fst . B16.decode . B.fromString . filter (not.isSpace) . map toLower
