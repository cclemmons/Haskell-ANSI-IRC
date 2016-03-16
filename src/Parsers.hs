{-# LANGUAGE OverloadedStrings #-}
module Parsers where

import System.IO
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bstr
import Data.Char
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as C8

data Command = Leave ByteString | Join ByteString | Quit | Help | Users | Resize | Clear | Malformed

hGetString :: Handle -> IO ByteString
hGetString hand = do
    mode <- hGetBuffering hand
    hSetBuffering hand NoBuffering
    str <- Bstr.hGetSome hand 10000
    hSetBuffering hand mode
    return str

windowSize :: Parser (Int, Int)
windowSize = (,) <$> (string "\ESC[" *> C8.decimal) <* string ";" <*> C8.decimal

parseWindowSize :: ByteString -> Maybe (Int, Int)
parseWindowSize = maybeResult . (parse windowSize)

command :: Parser Command
command = (Leave <$> (string ":leave " *> C8.takeTill isSpace)) <|>
          (Join <$> (string ":join " *> C8.takeTill isSpace)) <|>
          (const Quit <$> string ":quit") <|>
          (const Help <$> string ":help") <|>
          (const Users <$> string ":users") <|>
          (const Resize <$> string ":resize") <|>
          (const Clear <$> string ":clear") <|>
          (const Malformed <$> string ":")

parseCommand :: ByteString -> Maybe Command
parseCommand = maybeResult . (parse command)

userNameParser :: Parser ByteString
userNameParser = C8.takeWhile isAlphaNum

parseUserName :: ByteString -> Maybe ByteString
parseUserName = maybeResult . (parse userNameParser)