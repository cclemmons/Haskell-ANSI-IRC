{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( mainApp
    ) where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Monad
import Data.Monoid ((<>))

import Data.Time.Format
import Data.Time.Calendar
import Data.Time.Clock
import Data.List (sort)

import System.IO
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Network (listenOn, accept, PortID)
import Network.Socket (Socket)

import Data.ByteString (ByteString, pack)
import qualified Data.ByteString as Bstr
import Data.ByteString.Builder

msgTimeOut :: Int
msgTimeOut = 100

data User = User { username :: ByteString
                 , userHndl :: Handle
                 , userActv :: ByteString
                 , userRooms :: Map ByteString (TChan Msg)}
    deriving (Eq)

data Msg = Msg { msgTime :: UTCTime
               , msgSender :: ByteString
               , msgRoom :: ByteString
               , msgCont :: ByteString}
    deriving (Eq, Ord)

--announce :: User -> IO ()
--announce user = do
--    let broadcast = systemMsg (username user ++ " has entered")
--    atomically $ Map.foldl (\acc room -> do acc >> broadcast room) (return ()) (userRooms user)
--    where
--        foldfn acc room = do
            

systemMsg :: ByteString -> ByteString -> IO Msg
systemMsg str room = do
    t <- getCurrentTime
    return $ Msg t "System" room str

utcToBuilder :: UTCTime -> Builder
utcToBuilder t = stringUtf8 $ formatTime defaultTimeLocale "%R" t
    --where
    --    integerDec year <>. "-" <> intDec mon <>. "-" <> intDec day
    --    (year, mon, day) = toGregorian $ utctDay t
    --    a <>. b = a <> byteString b

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
    foldl (\acc new -> acc >> (hPutBuilder handle $ msgToBuilder new)) (return ()) msgs

getMsgs :: User -> STM [Msg]
getMsgs user = liftM sort $ Map.foldl (concat) (return []) (tchs)
    where concat acc tc = liftM2 (++) acc $ qdMsgs tc
          tchs = userRooms user

qdMsgs :: TChan Msg -> STM [Msg]
qdMsgs tc = do
    elem <- tryReadTChan tc
    case elem of
        Nothing -> return []
        Just x -> liftM2 (:) (return x) (qdMsgs tc)

userHandler :: User -> IO ()
userHandler user = do
    let handle = userHndl user
    ready <- hWaitForInput handle msgTimeOut
    if ready then recvMsg user else return ()
    sendMsgs user
    userHandler user

recvMsg :: User -> IO ()
recvMsg user = do
    bstr <- Bstr.hGetLine $ userHndl user
    t <- getCurrentTime
    let msg = Msg t (username user) (userActv user) bstr
    atomically $ writeTChan ((userRooms user) Map.! (userActv user)) msg

dupRooms :: Map ByteString (TChan Msg) -> STM (Map ByteString (TChan Msg))
dupRooms = Map.foldrWithKey foldfn (return Map.empty)
    where
        foldfn key val acc = do
            acc' <- acc
            val' <- dupTChan val
            return $ Map.insert key val' acc'

newUser :: Handle -> MVar (Map ByteString (TChan Msg))-> IO ()
newUser hand rooms' = do
    name <- getUsername hand
    rooms <- readMVar rooms'
    --room <- getUserRooms hand
    usrrooms <- atomically $ dupRooms rooms
    let user = User name hand "" usrrooms
    --announce user
    userHandler user

-- prompts user for name
getUsername :: Handle -> IO ByteString
getUsername hand = do
    Bstr.hPut hand "Please Enter a Username: "
    name <- Bstr.hGetLine hand
    return name

getUserRooms :: Handle -> IO (Set ByteString)
getUserRooms hand = return $ Set.singleton ("" :: ByteString)

acceptAndFork :: Socket -> MVar (Map ByteString (TChan Msg)) -> IO ThreadId
acceptAndFork sock rooms = do
    (hand, _, _) <- accept sock
    forkIO $ newUser hand rooms

addRoom :: ByteString -> Map ByteString (TChan Msg) -> STM (Map ByteString (TChan Msg))
addRoom name map = do
    tc <- newBroadcastTChan
    return $ Map.insertWith (flip const) name tc map

mainApp :: PortID -> IO ()
mainApp port = do
    startingRooms <- (atomically $ addRoom "" Map.empty)
    rooms <- newMVar startingRooms 
    sock <- listenOn port
    forever $ acceptAndFork sock rooms
