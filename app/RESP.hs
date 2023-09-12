{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module RESP where

import Control.Applicative ((<|>))
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (stringUtf8, toLazyByteString)
import Data.ByteString.Char8 (pack)
import Data.Functor (($>))
import Data.Functor.Identity (Identity)
import Text.Parsec (anyChar, count, digit, many1, option, parse, string)
import Text.Parsec.ByteString (Parser)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (manyTill)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (GenTokenParser, TokenParser, integer, makeTokenParser, reservedNames)
import Utils (toBSSlow)

data Resp
    = Str ByteString
    | BulkStr ByteString
    | Array Integer [Resp]
    | NullArray
    | NullBulk
    deriving (Eq, Ord, Show)

crlf' :: ByteString
crlf' = "\r\n"

signedIntParser :: Parser Int
signedIntParser = do
    sign <- option '+' (char '+' <|> char '-')
    digits <- many1 digit
    let intValue = read digits :: Int
    return $ if sign == '-' then -intValue else intValue

encode :: Resp -> Either ByteString ByteString
encode (Str x) = Right $ "+" <> x <> crlf'
encode NullBulk = Right "$-1\r\n"
encode r = Left $ "Don't know how to encode this" <> toBSSlow r

decode :: ByteString -> Either ParseError Resp
decode = parse resp ""

-- | An attoparsec parser to parse a single 'Resp' value.
resp :: Parser Resp
resp = do
    t <- anyChar
    case t of
        '+' -> Str <$> bytes <* crlf
        '*' -> array
        '$' -> bulkString
        _ -> fail $ "invalid type tag: " ++ show t

bytes :: Parser ByteString
bytes = pack <$> manyTill anyChar (char '\r')

crlf :: Parser ()
crlf = (char '\r' *> char '\n') $> ()

array :: Parser Resp
array = do
    n <- signedIntParser <* crlf
    if
            | n >= 0 -> Array (fromIntegral n) <$> replicateM n resp
            | n == -1 -> return NullArray
            | otherwise -> fail "negative array length"

bulkString :: Parser Resp
bulkString = do
    n <- signedIntParser <* crlf
    if
            | n >= 0 -> BulkStr . pack <$> count n anyChar <* crlf
            | n == -1 -> return NullBulk
            | otherwise -> fail "negative bulk length"
