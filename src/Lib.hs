{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan

import Network.Simple.TCP
import qualified Network.Socket as NSock
import System.IO
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Bytestring (Bytestring)
import qualified Data.Bytestring as Bstr

data User = User { username :: Bytestring
                 , userSocket :: Socket
                 , userHost :: SockAddr
                 , nextMsg :: Condition
                 , userRooms :: Map Bytestring Chan
                 , userPresence :: Mutex}

data Room = Room { roomName :: Bytestring
                 , roomMsgQ :: Chan
                 , roomUsers :: MVar Set User}

instance Eq Room where
    x == y = roomName x == roomName y

data Mutex = MVar ()

data Condition = MVar String

-- on user startup
getUser :: Socket -> SockAddr -> IO User
getUser sock addr = do
    name <- getUsername sock
    return $ User name sock addr

-- prompts user for name
getUsername :: Socket -> IO Bytestring
getUsername sock = do
    send "Please Enter a Username: "
    (name, len) <- recvLen 16
    if len <= 16 return name else return $ getUsername sock

sendUser :: Bytestring -> IO ()
sendUser = send . userSocket

getMsg :: User -> IO ByteString
getMsg = takeMvar . nextMsg

sendUserMsg :: User -> Bytestring -> IO ()
sendUserMsg user = sendUser user $ getMsg user

userloop :: User -> Set Room -> IO ()
userloop user rooms = do
    sendUserMsg user
    msg <- recvMsg user
    handleMsg msg user
    userloop

userHandler :: IO ()
userHandler sock addr = do
    newuser <- getUser sock addr
    userloop newuser

handleMsg :: Maybe Bytestring -> IO ()
handleMsg msg user = do
    Map.map (\ch -> writeChan ch msg) (userRooms user)


newRoom :: Bytestring -> IO Room
newRoom str = do
    ch <- newChan
    let room = Room str ch
    forkIO $ roomHandler room
    return room

roomHandler :: Room -> IO ()
roomHandler room = do

mainapp :: IO ()
mainapp = do
    users = newEmptyMVar
    rooms = Set.singleton $ newRoom ""
    serve (Host "12.7.0.0.1") "80" (newUser rooms users)

main :: IO ()
main = serve (Host "12.7.0.0.1") "80" userHandler

--netcat :: String -> String -> IO ()
--netcat host port = do
--  -- Extract address from first AddrInfo in list
--  AddrInfo{ addrAddress = addr, addrFamily = family }:_
--      <- getAddrInfo Nothing (Just host) (Just port)

--  -- Create a TCP socket connected to server
--  s <- socket family Stream 0
--  connect s addr

--  -- Convert socket to handle
--  h <- socketToHandle s ReadWriteMode
--  hSetBuffering h NoBuffering  -- THIS IS IMPORTANT

--  -- Punt on complex locale stuff
--  hSetBinaryMode stdout True

--  -- Copy data back and forth taking advantage of laziness
--  done <- newEmptyMVar
--  forkIO $ (hGetContents h >>= putStr) `finally` putMVar done ()
--  getContents >>= hPutStr h
--  takeMVar done



--data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

--data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

---- | @outcome our_move their_move@
--outcome :: Move -> Move -> Outcome
--outcome Rock Scissors        = Win
--outcome Paper Rock           = Win
--outcome Scissors Paper       = Win
--outcome us them | us == them = Tie
--                | otherwise  = Lose

--parseMove :: String -> Maybe Move
--parseMove str = case reads str of
--  [(m, rest)] | ok rest -> Just m
--  _                     -> Nothing
--  where ok = all (`elem` " \r\n")

--getMove :: Handle -> IO Move
--getMove h = do
--  hPutStrLn h $ "Please enter one of " ++ show ([minBound..] :: [Move])
--  input <- hGetLine h
--  case parseMove input of Just move -> return move
--                          Nothing -> getMove h

--computerVsUser :: Move -> Handle -> IO ()
--computerVsUser computerMove h = do
--  userMove <- getMove h
--  let o = outcome userMove computerMove
--  hPutStrLn h $ "You " ++ show o

--withTty :: (Handle -> IO a) -> IO a
--withTty = withFile "/dev/tty" ReadWriteMode

--withClient :: PortID -> (Handle -> IO a) -> IO a
--withClient listenPort fn = do
--  s <- listenOn listenPort
--  (h, host, port) <- accept s
--  putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
--  sClose s  -- Only accept one client
--  a <- fn h
--  hClose h
--  return a

--main :: IO ()
--main = withClient (PortNumber 3000) (chatClient)

--module Main where
--import Control.Concurrent
--import Control.Exception
--import Network
--import System.IO

--data Move = Rock | Paper | Scissors deriving (Eq, Read, Show, Enum, Bounded)

--data Outcome = Lose | Tie | Win deriving (Show, Eq, Ord)

--parseMove :: String -> Maybe Move
--parseMove str = case reads str of
--  [(m, rest)] | ok rest -> Just m
--  _                     -> Nothing
--  where ok = all (`elem` " \r\n")

--getMove :: Handle -> IO Move
--getMove h = do
--  hPutStrLn h $ "Please enter one of " ++ show ([minBound..] :: [Move])
--  input <- hGetLine h
--  case parseMove input of Just move -> return move
--                          Nothing -> getMove h

--computerVsUser :: Move -> Handle -> IO ()
--computerVsUser computerMove h = do
--  userMove <- getMove h
--  let o = outcome userMove computerMove
--  hPutStrLn h $ "You " ++ show o

--withTty :: (Handle -> IO a) -> IO a
--withTty = withFile "/dev/tty" ReadWriteMode

--withClient :: PortID -> (Handle -> IO a) -> IO a
--withClient listenPort fn =
--  bracket (listenOn listenPort) sClose $ \s -> do
--    bracket (accept s) (\(h, _, _) -> hClose h) $
--      \(h, host, port) -> do
--        putStrLn $ "Connection from host " ++ host ++ " port " ++ show port
--        fn h

---- You may find defining this function useful
--play :: MVar Move -> MVar Move -> (Handle, HostName, PortNumber) -> IO ()
--play myMoveMVar opponentMoveMVar (h, host, port) = undefined

---- You should define this function
--netrock :: PortID -> IO ()
--netrock listenPort = undefined

--main :: IO ()
--main = netrock (PortNumber 1617)