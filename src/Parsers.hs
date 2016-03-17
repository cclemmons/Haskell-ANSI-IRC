{-# LANGUAGE OverloadedStrings #-}
module Parsers where

import System.IO
import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as Bstr
import Data.Char
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString.Char8 as C8

data Command = Leave ByteString | Join ByteString | Switch ByteString | Where | Quit | Help | Users | Resize | Clear | Malformed
    deriving (Show)

-- gets as much input as it can, but not very useful unless NoBuffering
-- actually works on the handle
hGetString :: Handle -> IO ByteString
hGetString hand = do
    mode <- hGetBuffering hand
    hSetBuffering hand NoBuffering
    str <- Bstr.hGetSome hand 10000
    hSetBuffering hand mode
    return str

-- parses a DSR character into a window size
windowSize :: Parser (Int, Int)
windowSize = (,) <$> (string "\ESC[" *> C8.decimal) <* string ";" <*> C8.decimal

-- executes the parseWindowSize parser
parseWindowSize :: ByteString -> Maybe (Int, Int)
parseWindowSize = maybeResult . (parse windowSize)

-- parses the command
command :: Parser Command
command = (Leave <$> (string ":leave " *> C8.takeTill isSpace)) <|>
          (Join <$> (string ":join " *> C8.takeTill isSpace)) <|>
          (Switch <$> (string ":switch " *> C8.takeTill isSpace)) <|>
          (const Where <$> string ":where") <|>
          (const Quit <$> string ":quit") <|>
          (const Help <$> string ":help") <|>
          (const Users <$> string ":users") <|>
          (const Resize <$> string ":resize") <|>
          (const Clear <$> string ":clear") <|>
          (const Malformed <$> string ":")

-- executes the command parser
parseCommand :: ByteString -> Maybe Command
parseCommand = undo . (parseOnly command)
    where
        undo (Left _) = Nothing
        undo (Right comm) = Just comm

-- parses a username
userNameParser :: Parser ByteString
userNameParser = (C8.takeWhile1 isAlphaNum)

-- executes the username Parser, returning "" if it fails
parseUserName :: ByteString -> ByteString
parseUserName = undo . (parseOnly userNameParser)
    where
        undo (Left _) = ""
        undo (Right str) = str