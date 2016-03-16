{-# LANGUAGE OverloadedStrings #-}
module Parsers where

import System.IO
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bstr
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8

hGetString :: Handle -> IO ByteString
hGetString hand = do
    mode <- hGetBuffering hand
    hSetBuffering hand NoBuffering
    str <- Bstr.hGetSome hand 10000
    hSetBuffering hand mode
    return str

windowSize :: Parser (Int, Int)
windowSize = (,) <$> (string "\ESC[" *> decimal) <* string ";" <*> decimal

parseWindowSize :: ByteString -> Maybe (Int, Int)
parseWindowSize = maybeResult . (parse windowSize)

