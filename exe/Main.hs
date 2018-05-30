{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (unpack, pack)
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Data.Bool

import LambdaCoin.Node

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 8333 iNADDR_ANY)
    listen sock 5
    chan <- newChan
    mainLoop sock chan

mainLoop :: Socket -> Chan (SockAddr, Msg) -> IO ()
mainLoop sock chan = do
    conn <- accept sock
    forkIO $ runConn conn chan 
    mainLoop sock chan

runConn :: (Socket, SockAddr) -> Chan (SockAddr, Msg) -> IO ()
runConn (sock, addr) chan = do
    let writeConn msg = send sock (encode msg) >> return ()
        broadcast msg = writeChan chan (addr, msg)
        readConn = unpack <$> (isConnected sock >>= bool (return "") (recv sock 0x10000))

    myCopyOfChan <- dupChan chan

    reader <- forkIO $ fix $ \loop -> do
        (fromUser, msg) <- readChan myCopyOfChan
        when (fromUser /= addr) $ writeConn msg
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        msg <- readConn
        case msg of
            "" -> killThread reader
            _ -> processMsg >> broadcast msg >> loop

gracefulDisconnect :: Socket -> IO ()
gracefulDisconnect sock = do
    shutdown sock ShutdownBoth
    close sock
