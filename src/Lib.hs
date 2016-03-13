{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Data.Monoid

import Data.Time.Calendar
import Data.Time.Clock

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network.Socket

import Data.ByteString (ByteString)
import qualified Data.ByteString as Bstr
import Data.ByteString.Builder

msgTimeOut :: Int
msgTimeOut = 100

data User = User { username :: ByteString
                 , userHndl :: Handle
                 , userActv :: ByteString
                 , userRooms :: Map ByteString (TChan Msg)}
    deriving (Show, Eq)

data Msg = Msg { msgTime :: UTCTime
               , msgSender :: ByteString
               , msgRoom :: ByteString
               , msgCont :: ByteString}
    deriving (Show, Eq, Ord)

utcToBuilder :: UTCTime -> Builder
utcToBuilder t = stringUtf8 $ utctDay t

msgToBuilder :: Msg -> Builder
msgToBuilder msg = byteString "<" <> username <>. " " <> room <> time <>. ">" <>." " <> cont <>. "\n"
    where
        username = byteString $ msgSender msg
        room = byteString $ msgRoom msg
        cont = byteString $ msgCont msg
        time = utcToBuilder $ msgTime msg
        a <>. b = a <> byteString b

sendMsgs :: User -> IO ()
sendMsgs user = do
    let handle = userHndl user
    msgs <- atomically $ getMsgs user
    foldl () ((>>) . hPutBuilder . msgToBuilder) msgs

getMsgs :: User -> STM [Msg]
getMsgs user = liftM sort $ Map.foldr (concat) (return []) (tchs)
    where concat acc tc = liftM2 (++) acc $ qdMsgs tc
          tchs = userRooms user

qdMsgs :: TChan ByteString -> STM [Msg]
qdMsgs tc = do
    elem <- tryReadTChan tc
    return $ case elem of
                Nothing -> [] 
                Just x -> (qdMsgs tc):x[]

userHandler :: User -> IO ()
userHandler user = do
    let handle = userHndl user
    ready <- hWaitForInput handle msgTimeOut
    if ready then recvMsg user else ()
    sendMsgs user
    userHandler user

recvMsg :: User -> IO ()
recvMsg user = do
    bstr <- Bstr.hGetLine
    t <- getCurrentTime
    let msg = Msg t (username user) (userActv user) bstr
    atomically $ writeTchan ((userRooms user) Map.! (userActv user)) msg

newUser :: Handle -> MVar (Map ByteString (TChan Msg))-> IO ()
newUser hand rooms' = do
    name <- getUsername hand
    rooms <- readMVar rooms'
    --room <- getUserRooms hand
    userHandler $ User name hand "" rooms

-- prompts user for name
getUsername :: Handle -> IO ByteString
getUsername hand = do
    Bstr.hPut hand "Please Enter a Username: "
    name <- Bstr.hGetLine hand
    return name

getUserRooms :: Handle -> IO (Set ByteString)
getUserRooms = return $ Set.singleton ""

acceptAndFork :: Socket -> MVar (Map ByteString (TChan Msg)) -> IO ()
acceptAndFork sock rooms = do
    (hand, _, _) <- NSock.accept sock
    forkIO $ newUser hand rooms

addRoom :: ByteString -> Map ByteString (TChan Msg) -> STM (Map ByteString (TChan Msg))
addRoom name map = do
    tc <- newTChan
    return $ Map.insertWith (flip const) name tc map

mainapp :: String -> IO ()
mainapp port = do
    startingRooms <- (atomically $ addRoom "" Map.Empty)
    let rooms = newMVar startingRooms 
    sock <- listenOn port
    forever $ acceptAndFork sock rooms
