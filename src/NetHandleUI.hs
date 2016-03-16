{-# LANGUAGE OverloadedStrings #-}
module NetHandleUI where

import System.IO
import System.Console.ANSI
import Data.ByteString (ByteString)
import Data.List
import qualified Data.ByteString as Bstr
import Data.ByteString.Builder
import qualified Data.ByteString.Read as Read

import Parsers

data Window = Window { wWidth :: Int
                     , wHeight :: Int
                     , wHandle :: Handle}
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Adding functionality to ANSI terminal

-- converts arguments and codes into an escape sequence
-- taken from System.Console.ANSI source
csi :: [Int] -> String -> String
csi args code = "\ESC[" ++ concat (intersperse ";" (map show args)) ++ code

--saveCursorCode :: String
--saveCursorCode = csi [] "s"

--restoreCursorCode :: String
--restoreCursorCode = csi [] "u"

--hSaveCursor :: Handle -> IO ()
--hSaveCursor h = hPutStr h saveCursorCode

--hRestoreCursor :: Handle -> IO ()
--hRestoreCursor h = hPutStr h restoreCursorCode

hGetSize :: Handle -> IO ()
hGetSize hand = hPutStr hand dsr

dsr :: String
dsr = csi [6] "n"
------------------------------------------------------------------------

wScrollPageDown :: Window -> Int -> IO ()
wScrollPageDown w n = hScrollPageDown (wHandle w) n

getInt :: ByteString -> Handle -> IO Int
getInt str hand = do
    Bstr.hPut hand $ Bstr.concat ["Please Enter Your ", str, " : "]
    var <- Bstr.hGetLine hand
    let width = Read.int var
    case width of
        Nothing -> getInt str hand
        Just (w, _) -> return w

hGetWindow :: Handle -> IO Window
hGetWindow hand = do
    hClearScreen hand
    hSetCursorPosition hand 1 0
    Bstr.hPut hand "Please press enter."
    hSetCursorPosition hand 1000000 1000000
    hGetSize hand
    str <- hGetString hand
    let Just (row, col) = parseWindowSize str
    hClearScreen hand
    hSetCursorPosition hand 0 0
    return $ Window col row hand

newInput :: Window -> IO ()
newInput w = hSetCursorPosition (wHandle w) (wHeight w) 0

sendBuilderOffset :: Window -> Builder -> Int -> IO ()
sendBuilderOffset w bld off = do
    hScrollPageDown (wHandle w) off
    newInput w
    hPutBuilder (wHandle w) bld
    newInput w

sendBuilder :: Window -> Builder -> IO ()
sendBuilder w bld = sendBuilderOffset w bld 0

linesInput :: Window -> ByteString -> Int
linesInput w st = negate $ nlength `div` width
    where
        nlength = negate $ Bstr.length st
        width = wWidth w

clearWindow :: Window -> IO ()
clearWindow w = hClearScreen $ wHandle w