{-# LANGUAGE OverloadedStrings #-}
module NetHandleUI where

import System.IO
import System.Console.ANSI
import Data.ByteString (ByteString)
import Data.List
import qualified Data.ByteString as Bstr
import Data.ByteString.Builder
import qualified Data.ByteString.Read as Read

data Window = Window {wWidth :: Int
                     , wHeight :: Int
                     , wHandle :: Handle}
    deriving (Show)

------------------------------------------------------------------------
-- Adding functionality to ANSI terminal

-- converts arguments and codes into an escape sequence
-- taken from System.Console.ANSI source
csi :: [Int] -> String -> String
csi args code = "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code

saveCursorCode :: String
saveCursorCode = csi [] "s"

restoreCursorCode :: String
restoreCursorCode = csi [] "u"

hSaveCursor :: Handle -> IO ()
hSaveCursor h = hPutStr h saveCursorCode

hRestoreCursor :: Handle -> IO ()
hRestoreCursor h = hPutStr h restoreCursorCode
------------------------------------------------------------------------

getInt :: ByteString -> Handle -> IO Int
getInt str hand = do
    Bstr.hPut hand $ Bstr.concat ["Please Enter Your ", str, " : "]
    var <- Bstr.hGetLine hand
    let width = Read.int var
    case width of
        Nothing -> getInt str hand
        Just (w, _) -> return w

getWindow :: Handle -> IO Window
getWindow hand = do
    mode <- hGetBuffering hand
    hSetBuffering hand LineBuffering
    width <- getInt "Console Width" hand
    height <- getInt "Console Height" hand
    hSetBuffering hand mode
    return $ Window width height hand

newInput :: Window -> IO ()
newInput w = hSetCursorPosition (wHandle w) (wHeight w) 0

sendMsg :: Window -> Builder -> IO ()
sendMsg w bld = do
    newInput w
    hPutBuilder (wHandle w) bld
    newInput w
    hScrollPageUp (wHandle w) 1
