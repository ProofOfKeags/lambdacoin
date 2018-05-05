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

type Msg = (Int, String)

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 8333 iNADDR_ANY)
    listen sock 5
    chan <- newChan
    mainLoop sock chan 0

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
    conn <- accept sock
    forkIO $ runConn conn chan msgNum
    mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan user = do
    let writeConn msg = send sock (pack msg) >> return ()
        broadcast msg = writeChan chan (user, msg)
        readConn = unpack <$> (isConnected sock >>= bool (return "") (recv sock 0x10000))

    writeConn "Hi, what is your name?\n"
    name <- init . init <$> readConn
    putStrLn $ "Welcome " ++ name ++ "!\n"
    broadcast ("--> " ++ name ++ " entered chat\n")
    writeConn $ "Welcome " ++ name ++ "!\n"

    myCopyOfChan <- dupChan chan

    reader <- forkIO $ fix $ \loop -> do
        (fromUser, msg) <- readChan myCopyOfChan
        when (fromUser /= user) $ writeConn msg
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        msg <- readConn
        case msg of
            "" -> putStrLn (name ++ " disconnected") >> killThread reader
            "quit" -> writeConn "Bye" >> killThread reader >> gracefulDisconnect sock
            _ -> putStrLn msg >> broadcast (name ++ ": " ++ msg) >> loop

gracefulDisconnect :: Socket -> IO ()
gracefulDisconnect sock = do
    shutdown sock ShutdownBoth
    close sock
