module Main where

import Control.Monad (forever)
import Network.Socket

main :: IO ()
main = do
    sock <- serveSocket 3000
    listen sock 5
    forever $ do
        (conn, addr) <- accept sock
        putStrLn $ "接続: " ++ show addr
        close conn

serveSocket :: PortNumber -> IO Socket
serveSocket port = do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock (SockAddrInet port (tupleToHostAddress (0,0,0,0)))
    return sock
