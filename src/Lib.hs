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
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as Bstr
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Network (listenOn, accept, PortID)
import Network.Socket (Socket)

import System.IO

import NetHandleUI
import Parsers

type Rooms = Map ByteString (TChan Msg)

data Server = Server { srvUsers :: MVar (Set User)
                     , srvRooms :: MVar Rooms}

data User = User { userName :: ByteString
                 , userHndl :: Handle
                 , userActv :: ByteString
                 , userRooms :: Rooms
                 , userWndw :: Window
                 , server :: Server}
instance Ord User where
    compare a b = compare (userName a) (userName b)
    (<=) a b = (<=) (userName a) (userName b)
instance Eq User where
    (==) a b = (userName a) == (userName b)
instance Show User where
    show = unpack . userName

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
recvMsg user@(User name handle active rooms window _) = do
    bstr <- Bstr.hGetLine handle
    case parseCommand bstr of
        Just command -> executeCommand user command
        Nothing -> do
            t <- getCurrentTime
            let msg = Msg t name active bstr
            atomically $ writeTChan (rooms Map.! active) msg
    wScrollPageDown window $ linesInput window bstr

help :: ByteString
help = Bstr.concat
    [ "Welcome to the Chat!\n"
    , "This Chat client supports ANSI terminals as long as they support scrolling\n"
    , "There is no way to properly handle buffered input as you "
    , "recieve messages, note that if you are composing a message and "
    , "recieve a message, your work is likely to disappear, but it will "
    , "likely reappear with a backspace.\n\n"
    , "The following commands are supported:\n"
    , "\":quit\" to leave (You can also use ^D\n"
    , "\":help\" to see this message again\n"
    , "\":leave name\" to leave the chatroom by name\n"
    , "\":join name\" to join a chatroom by name\n"
    , "\":users\" to see a list of users online\n"
    , "\":resize\" to get send your window size to the server for proper UI\n"
    , "\":clear\" to clear your screen\n"]

executeCommand :: User -> Command -> IO ()
executeCommand user (Leave room) = do
    undefined
executeCommand user (Join room) = do
    undefined
executeCommand user Quit = myThreadId >>= killThread
executeCommand user Help = systemMsg help "" >>= sendMsg user
executeCommand user Users = do
    users <- readMVar $ srvUsers $ server user
    systemMsg (pack $ show $ Set.toList users) "" >>= sendMsg user
executeCommand user Resize = do
    undefined
executeCommand user Clear = clearWindow $ userWndw user
executeCommand user Malformed = executeCommand user Help

-- Sends a message to someone's window
sendMsg :: User -> Msg -> IO ()
sendMsg user msg = sendBuilder window bld
    where
        bld = msgToBuilder msg
        window = userWndw user

-- Generates a new map of rooms with duped tchans
dupRooms :: Rooms -> STM (Rooms)
dupRooms = Map.foldrWithKey foldfn (return Map.empty)
    where
        foldfn key val acc = do
            acc' <- acc
            val' <- dupTChan val
            return $ Map.insert key val' acc'

-- User initialization that passes off the user to the user handler
newUser :: Handle -> Server -> IO ()
newUser hand serve = do
    window <- hGetWindow hand
    name <- getUsername hand
    rooms <- readMVar $ srvRooms serve
    --room <- getUserRooms hand
    usrrooms <- atomically $ dupRooms rooms
    let user = User name hand "" usrrooms window serve
    systemMsg help "" >>= sendMsg user
    bracket (birthUser user) (killUser) (userHandler)

birthUser :: User -> IO User
birthUser user = do
    let (Server users _) = server user
    announce user " has joined"
    addUser user
    return user

killUser :: User -> IO ()
killUser user = do
    let (Server users _) = server user
    announce user " has left"
    bracket (takeMVar users) (putMVar users) (return . (Set.delete user))
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
acceptAndFork :: Socket -> Server -> IO ThreadId
acceptAndFork sock serve = do
    (hand, _, _) <- accept sock
    forkIO $ bracket (return hand) (hClose) (flip newUser serve)

-- adds a new room by name to a map of all available rooms
newRoom :: ByteString -> Rooms -> STM (Rooms)
newRoom name map = do
    tc <- newBroadcastTChan
    return $ Map.insertWith (flip const) name tc map

-- runs the chat server on a given Port. Initializes a default room
mainApp :: PortID -> IO ()
mainApp port = do
    startingRooms <- (atomically $ newRoom "" Map.empty)
    rooms <- newMVar startingRooms 
    users <- newMVar Set.empty
    let serve = Server users rooms
    sock <- listenOn port
    forever $ acceptAndFork sock serve

addRoom :: Server -> ByteString -> IO ()
addRoom serve@(Server users rooms) name = 
    bracketOnError (takeMVar rooms) (putMVar rooms) (\r -> (atomically $ newRoom name r) >>= putMVar rooms)

addUser :: User -> IO ()
addUser user = let serve@(Server users rooms) = server user in
    bracketOnError (takeMVar users) (putMVar users) ((putMVar users) . Set.insert user)