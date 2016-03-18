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
                 , userActv :: MVar ByteString
                 , userRooms :: MVar Rooms
                 , userWndw :: MVar Window
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
    rooms <- readMVar $ userRooms user
    Map.foldlWithKey foldfn (return ()) rooms
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
msgToBuilder msg = byteString "<" <> username <>. "@" <> room <>. " " <> time <>. ">" <>." " <> cont <>. "\n"
    where
        username = byteString $ msgSender msg
        room = byteString $ msgRoom msg
        cont = byteString $ msgCont msg
        time = utcToBuilder $ msgTime msg
        a <>. b = a <> byteString b

-- Flushes all the messages from user's rooms to their handle
sendMsgs :: User -> IO ()
sendMsgs user = do
    window <- readMVar $ userWndw user
    rooms <- readMVar $ userRooms user
    msgs <- atomically $ getMsgs rooms
    foldl (\acc new -> acc >> (sendMsg user new)) (return ()) msgs

-- generates a list of Msgs from a user's rooms
getMsgs :: Rooms -> STM [Msg]
getMsgs rooms = liftM sort $ Map.foldl (concat) (return []) (rooms)
    where concat acc tc = liftM2 (++) acc $ dQMsgs tc

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
recvMsg user@(User name handle active' rooms' window' _) = do
    bstr <- Bstr.hGetLine handle
    active <- readMVar active'
    rooms <- readMVar rooms'
    window <- readMVar window'
    wScrollPageDown window $ (linesInput window bstr)
    wClearLine window
    case parseCommand bstr of
        Just command -> executeCommand user command
        Nothing -> do
            t <- getCurrentTime
            let msg = Msg t name active bstr
            atomically $ writeTChan (rooms Map.! active) msg

-- This is the help message
help :: ByteString
help = Bstr.concat
    [ "Welcome to the Chat!\n"
    , "This Chat client supports ANSI terminals as long as they support scrolling\n"
    , "There is no way to properly handle buffered input as you "
    , "recieve messages, note that if you are composing a message and "
    , "recieve a message, your work is likely to disappear, but it will "
    , "likely reappear with a backspace.\n\n"
    , "You enter the chat in the default chat \"\" (empty string),\n"
    , "You can follow as many chats at once as you like, but your\n"
    , "messages will only be sent to one at a time\n"
    , "The message header is <User@room hh:mm>\n"
    , "The following commands are supported:\n"
    , "\":quit\" to leave (You can also use ^D\n"
    , "\":help\" to see this message again\n"
    , "\":leave name\" to leave the chatroom by name\n"
    , "\":join name\" to join a chatroom by name\n"
    , "\":switch name\" to switch your active room to a given room\n"
    , "\":where\" to tell you what your current active room is\n"
    , "\":users\" to see a list of users online\n"
    , "\":resize\" to re-send your window size to the server for proper UI, also clears screen\n"
    , "\":clear\" to clear your screen\n"]

-- Executes a command for a given user
executeCommand :: User -> Command -> IO ()
executeCommand user (Leave room) = do
    let mv = userRooms user
    bracketOnError (takeMVar mv) (putMVar mv) (\rooms -> do
        let rooms' = Map.delete room rooms
        putMVar mv rooms')
executeCommand user (Join room) = joinRoom user room
executeCommand user (Switch room) = do
    let actvMv = userActv user
    rooms <- readMVar $ userRooms user
    if room `Map.member` rooms then
            swapMVar actvMv room >> return ()
        else
            systemMsg "You cannot switch to a room you are not a member of." "" >>= sendMsg user
executeCommand user Where = do
    activ <- readMVar $ userActv user
    systemMsg (Bstr.concat ["Your active room is :", activ]) "" >>= sendMsg user
executeCommand user Quit = myThreadId >>= killThread
executeCommand user Help = systemMsg help "" >>= sendMsg user
executeCommand user Users = do
    users <- readMVar $ srvUsers $ server user
    systemMsg (pack $ show $ Set.toList users) "" >>= sendMsg user
executeCommand user Resize = do
    let mv = userWndw user
        hand = userHndl user
    bracketOnError (takeMVar mv) (putMVar mv) (\_ -> do
        w <- hGetWindow hand
        putMVar mv w
        newInput w)

executeCommand user Clear = (readMVar $ userWndw user) >>= clearWindow
executeCommand user Malformed = executeCommand user Help

-- Adds a User to a given room and announces them
joinRoom :: User -> ByteString -> IO ()
joinRoom user room = do
    addRoom (server user) room
    serveRooms <- readMVar $ srvRooms $ server user
    let tch = serveRooms Map.! room
        mv = userRooms user
    bracketOnError (takeMVar mv) (putMVar mv) (\rooms -> do
        tch' <- atomically $ dupTChan tch
        let rooms' = Map.insert room tch' rooms
        putMVar mv rooms'
        msg <- systemMsg (Bstr.concat [userName user, " has joined"]) room
        atomically $ writeTChan tch' msg)

-- Sends a message to someone's window
sendMsg :: User -> Msg -> IO ()
sendMsg user msg = readMVar window >>= flip sendBuilder bld
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
    window <- hGetWindow hand >>= newMVar
    name <- getUsername hand serve
    usrrooms <- newMVar Map.empty
    active <- newMVar ""
    let user = User name hand active usrrooms window serve
    joinRoom user ""
    systemMsg help "" >>= sendMsg user
    bracket (birthUser user) (killUser) (userHandler)

-- Births a user by addint them to the server and announcing them
birthUser :: User -> IO User
birthUser user = do
    let (Server users _) = server user
    addUser user
    return user

-- Kills a user and shugs down their thread
killUser :: User -> IO ()
killUser user = do
    let (Server users _) = server user
    announce user " has left"
    removeUser user
    msg <- systemMsg (Bstr.concat ["Goodbye ", userName user]) ""
    sendMsg user msg

-- prompts user for name
getUsername :: Handle -> Server -> IO ByteString
getUsername hand serve = do
    Bstr.hPut hand "Please Enter an Alphanumeric Username: "
    name <- Bstr.hGetLine hand
    parsed <- checkUserName name serve
    let okay = (parsed /= "" && parsed /= "System")
    if okay then return parsed else Bstr.hPut hand "Username Taken or Invalid.\n" >> getUsername hand serve

checkUserName :: ByteString -> Server -> IO ByteString
checkUserName name server = do
    users <- readMVar $ srvUsers server
    let parsed = parseUserName name
    if Set.foldl' (\acc val -> acc || (userName val == parsed)) False users then return "" else return parsed

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

-- adds a room to the server by name
addRoom :: Server -> ByteString -> IO ()
addRoom serve@(Server users rooms) name = 
    bracketOnError (takeMVar rooms) (putMVar rooms) (\r -> (atomically $ newRoom name r) >>= putMVar rooms)

-- adds a given user to the server
addUser :: User -> IO ()
addUser user = let serve@(Server users rooms) = server user in
    bracketOnError (takeMVar users) (putMVar users) ((putMVar users) . Set.insert user)

-- removes a given user from the server
removeUser :: User -> IO ()
removeUser user = let serve@(Server users rooms) = server user in
    bracketOnError (takeMVar users) (putMVar users) ((putMVar users) . Set.delete user)
