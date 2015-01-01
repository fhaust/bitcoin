
module Bitcoin.Network where

import System.IO
import Network.Socket
import Control.Exception
import Control.Applicative

import qualified Data.ByteString as B

import Bitcoin.Protocol

---------------------------------
-- connect
connect :: AddrInfo -> IO Handle
connect a = do
  s <- socket (addrFamily a) (addrSocketType a) (addrProtocol a)
  Network.Socket.connect s (addrAddress a)
  socketToHandle s ReadWriteMode



---------------------------------
-- dns discover

dnsSeeds :: [HostName]
dnsSeeds = ["bitseed.xf2.org"
           ,"dnsseed.bluematt.me"
           ,"seed.bitcoin.sipa.be"
           ,"dnsseed.bitcoin.dashjr.org"
           ]

dnsDiscoverAll :: IO [AddrInfo]
dnsDiscoverAll = concat <$> mapM dnsDiscover dnsSeeds

dnsDiscover :: HostName -> IO [AddrInfo]
dnsDiscover h = do
  result <- try $ getAddrInfo 
    (Just defaultHints) 
    (Just h) 
    (Just "8333") :: IO (Either IOException [AddrInfo])
  case result of
    Left  _ -> return []
    Right v -> return v


handshake :: Handle -> IO ()
handshake = do

  nonce <- randomIO  

  
