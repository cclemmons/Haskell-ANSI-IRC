{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Data.Monoid ((<>))

import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Clock
import Data.List (sort)
import Data.ByteString.Builder
import Data.Set (Set)

import qualified Data.Set as Set
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bstr
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Network (listenOn, accept, PortID)
import Network.Socket (Socket)

import System.IO

import NetHandleUI

data Server = Server { users :: MVar [User]
                     , rooms :: MVar (Map ByteString (TChan Msg))}

data User = User { userName :: ByteString
                 , userHndl :: Handle
                 , userActv :: ByteString
                 , userRooms :: Map ByteString (TChan Msg)
                 , userWndw :: Window}
    deriving (Eq)

data Msg = Msg { msgTime :: UTCTime
               , msgSender :: ByteString
               , msgRoom :: ByteString
               , msgCont :: ByteString}
    deriving (Eq, Ord)

-- announces a user's entrance to all of their rooms
announce :: User -> ByteString -> IO User
announce user constr = do
    let broadcast = systemMsg (Bstr.concat [userName user, constr])
        foldfn acc roomname tchan = do
            acc
            msg <- broadcast roomname
            atomically $ writeTChan tchan msg
    Map.foldlWithKey foldfn (return ()) (userRooms user)
    return user

-- Generates a message from the system with the given room and text
systemMsg :: ByteString -> ByteString -> IO Msg
systemMsg str room = do
    t <- getCurrentTime
    return $ Msg t "System" room str

-- Transforms a UTCTime into a Bytestring Builder
utcToBuilder :: UTCTime -> Builder
utcToBuilder t = stringUtf8 $ formatTime defaultTimeLocale "%R" t

-- Formats a message into a bytestring builder
msgToBuilder :: Msg -> Builder
msgToBuilder msg = byteString "<" <> username <>. " " <> room <> time <>. ">" <>." " <> cont <>. "\n"
    where
        username = byteString $ msgSender msg
        room = byteString $ msgRoom msg
        cont = byteString $ msgCont msg
        time = utcToBuilder $ msgTime msg
        a <>. b = a <> byteString b

-- Flushes all the messages from user's rooms to their handle
sendMsgs :: User -> IO ()
sendMsgs user = do
    let window = userWndw user
    msgs <- atomically $ getMsgs user
    foldl (\acc new -> acc >> (sendMsg user new)) (return ()) msgs

-- generates a list of Msgs from a user's rooms
getMsgs :: User -> STM [Msg]
getMsgs user = liftM sort $ Map.foldl (concat) (return []) (tchs)
    where concat acc tc = liftM2 (++) acc $ dQMsgs tc
          tchs = userRooms user

-- reads the messages from a given tchan into an list
dQMsgs :: TChan Msg -> STM [Msg]
dQMsgs tc = do
    elem <- tryReadTChan tc
    case elem of
        Nothing -> return []
        Just x -> liftM2 (:) (return x) (dQMsgs tc)

-- How long each user loop waits to see if there is input
msgTimeOut :: Int
msgTimeOut = 100

-- Final user thread function, checks for input and then recieves if
-- necessary and sends information to the user
userHandler :: User -> IO ()
userHandler user = do
    let handle = userHndl user
    ready <- hWaitForInput handle msgTimeOut
    if ready then recvMsg user else return ()
    sendMsgs user
    userHandler user

-- recieves a user's message and writes it to their active channel
recvMsg :: User -> IO ()
recvMsg user = do
    bstr <- Bstr.hGetLine $ userHndl user
    t <- getCurrentTime
    let msg = Msg t (userName user) (userActv user) bstr
    atomically $ writeTChan ((userRooms user) Map.! (userActv user)) msg
    wScrollPageDown (userWndw user) 1

sendMsg :: User -> Msg -> IO ()
sendMsg user msg = sendBuilder window bld
    where
        bld = msgToBuilder msg
        window = userWndw user

-- Generates a new map of rooms with duped tchans
dupRooms :: Map ByteString (TChan Msg) -> STM (Map ByteString (TChan Msg))
dupRooms = Map.foldrWithKey foldfn (return Map.empty)
    where
        foldfn key val acc = do
            acc' <- acc
            val' <- dupTChan val
            return $ Map.insert key val' acc'

-- User initialization that passes off the user to the user handler
newUser :: Handle -> MVar (Map ByteString (TChan Msg))-> IO ()
newUser hand rooms' = do
    name <- getUsername hand
    rooms <- readMVar rooms'
    --room <- getUserRooms hand
    usrrooms <- atomically $ dupRooms rooms
    window <- getWindow hand
    let user = User name hand "" usrrooms window
    bracket (announce user " has joined") (killUser) (userHandler)

killUser :: User -> IO ()
killUser user = do
    flip announce " has left" user
    msg <- systemMsg (Bstr.concat ["Goodbye ", userName user]) ""
    hPutBuilder (userHndl user) $ msgToBuilder $ msg

-- prompts user for name
getUsername :: Handle -> IO ByteString
getUsername hand = do
    Bstr.hPut hand "Please Enter a Username: "
    name <- Bstr.hGetLine hand
    return name

-- asks the user what rooms they would like to be a part of
getUserRooms :: Handle -> IO (Set ByteString)
getUserRooms hand = return $ Set.singleton ("" :: ByteString)

-- accepts a user and forks them into their own thread
acceptAndFork :: Socket -> MVar (Map ByteString (TChan Msg)) -> IO ThreadId
acceptAndFork sock rooms = do
    (hand, _, _) <- accept sock
    forkIO $ bracket (return hand) (hClose) (flip newUser rooms)

-- adds a new room by name to a map of all available rooms
addRoom :: ByteString -> Map ByteString (TChan Msg) -> STM (Map ByteString (TChan Msg))
addRoom name map = do
    tc <- newBroadcastTChan
    return $ Map.insertWith (flip const) name tc map

-- runs the chat server on a given Port. Initializes a default room
mainApp :: PortID -> IO ()
mainApp port = do
    startingRooms <- (atomically $ addRoom "" Map.empty)
    rooms <- newMVar startingRooms 
    sock <- listenOn port
    forever $ acceptAndFork sock rooms
