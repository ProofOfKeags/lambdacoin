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
import Control.Monad (forever, when)
import Control.Monad.Fix (fix)
import Data.Bool
import Data.Serialize

import LambdaCoin.Node
import LambdaCoin.Message

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 8333 iNADDR_ANY)
    listen sock 5
    chan <- newChan
    nodeState <- newMVar defaultNode
    mainLoop nodeState sock chan

mainLoop :: MVar Node -> Socket -> Chan (SockAddr, Msg) -> IO ()
mainLoop mNode sock chan = forever $ do
    conn <- accept sock
    forkIO $ runConn mNode conn chan 

runConn :: MVar Node -> (Socket, SockAddr) -> Chan (SockAddr, Msg) -> IO ()
runConn mNode (sock, addr) chan = do
    let writeConn msg = send sock (encode msg) >> return ()
        broadcast msg = writeChan chan (addr, msg)
        readConn = isConnected sock >>= bool (return "") (recv sock 0x10000)

    myCopyOfChan <- dupChan chan

    reader <- forkIO $ fix $ \loop -> do
        (fromUser, msg) <- readChan myCopyOfChan
        when (fromUser /= addr) $ writeConn msg
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        rawMsg <- readConn
        case rawMsg of
            "" -> killThread reader
            _ -> case decode rawMsg of
                Left _ -> loop
                Right msg -> do
                    nodeState <- takeMVar mNode
                    let processed = processMsg nodeState msg
                    case processed of
                        Nothing -> putMVar mNode nodeState
                        Just nodeState' -> do
                            putMVar mNode nodeState' 
                            broadcast msg
                            loop

gracefulDisconnect :: Socket -> IO ()
gracefulDisconnect sock = do
    shutdown sock ShutdownBoth
    close sock
