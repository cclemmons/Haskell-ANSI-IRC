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

-- Sends the dsr code to the Handle
hGetSize :: Handle -> IO ()
hGetSize hand = hPutStr hand dsrCode

dsrCode :: String
dsrCode = csi [6] "n"
------------------------------------------------------------------------

-- scrolls a window down a given number of lines
wScrollPageDown :: Window -> Int -> IO ()
wScrollPageDown _ 0 = return ()
wScrollPageDown w n = hScrollPageDown (wHandle w) n

-- Prompts handle for an int with the given prompt
getInt :: ByteString -> Handle -> IO Int
getInt str hand = do
    Bstr.hPut hand $ Bstr.concat ["Please Enter Your ", str, " : "]
    var <- Bstr.hGetLine hand
    let width = Read.int var
    case width of
        Nothing -> getInt str hand
        Just (w, _) -> return w

-- given a handle, retruns the appropriately sized window by using DSR
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

-- moves the cursor to the newinput location in the window
newInput :: Window -> IO ()
newInput w = hSetCursorPosition (wHandle w) (wHeight w) 0

-- sends a builder to a window, first scrolling down a given number of lines
sendBuilderOffset :: Window -> Builder -> Int -> IO ()
sendBuilderOffset w bld off = do
    hScrollPageDown (wHandle w) (off)
    newInput w
    hClearLine (wHandle w)
    hPutBuilder (wHandle w) bld
    hScrollPageUp (wHandle w) 1
    newInput w

-- sends a builder with a zero offset
sendBuilder :: Window -> Builder -> IO ()
sendBuilder w bld = sendBuilderOffset w bld 0

-- clears the line the cursors is on in the given window
wClearLine :: Window -> IO ()
wClearLine = hClearLine . wHandle

-- given a window and a bytestring, calculates the number of lines it took up
linesInput :: Window -> ByteString -> Int
linesInput w st = negate $ nlength `div` width
    where
        nlength = negate $ Bstr.length st
        width = wWidth w

-- clears the given window
clearWindow :: Window -> IO ()
clearWindow w = hClearScreen $ wHandle w