module L06.JsonParser where

--import Numeric
--import Control.Applicative
import L01.Validation
import L03.Parser
import L06.JsonValue
import L06.MoreParser

dQuoteChar :: Char
dQuoteChar = '"'

-- Exercise 1
-- Parse a JSON string. Handle double-quotes, control characters, hexadecimal characters.
-- ~~~ Use oneof, hex, is, satisfyAll, betweenCharTok, list ~~~
-- FIXME: Fully implement this with escaped control characters and hex characters.
jsonString :: Parser String
jsonString = between dquote dquote notQuote
  where
    notQuote = many0 (noneof [dQuoteChar])
    dquote = is dQuoteChar

-- Exercise 2
-- Parse a JSON rational.
-- ~~~ Use readSigned and readFloat ~~~
-- FIXME: Implement fully.
jsonNumber :: Parser Rational
jsonNumber = do 
  r <- digits 
  return (fromInteger r)

-- Exercise 3
-- Parse a JSON true literal.
-- ~~~ Use stringTok ~~~
jsonTrue :: Parser String
jsonTrue = stringTok "true"

-- Exercise 4
-- Parse a JSON false literal.
-- ~~~ Use stringTok ~~~
jsonFalse :: Parser String
jsonFalse = stringTok "false"

-- Exercise 5
-- Parse a JSON null literal.
-- ~~~ Use stringTok ~~~
jsonNull :: Parser String
jsonNull = stringTok "null"

-- Exercise 6
-- Parse a JSON array.
-- ~~~ Use betweenSepbyComma and jsonValue ~~~
jsonArray :: Parser [JsonValue]
jsonArray = betweenSepbyComma '[' ']' jsonValue

-- Exercise 7
-- Parse a JSON object.
-- ~~~ Use jsonString, charTok, betweenSepbyComma and jsonValue ~~~
jsonObject :: Parser Assoc
jsonObject = betweenSepbyComma '{' '}' pair
  where
    pair = do
      name <- jsonString
      spaces
      charTok ':'
      value <- jsonValue
      return (name, value)

-- Exercise 8
-- Parse a JSON value.
-- ~~~ Use spaces, jsonNull, jsonTrue, jsonFalse, jsonArray, jsonString, jsonObject and jsonNumber ~~~
jsonValue :: Parser JsonValue
jsonValue = do 
  spaces 
  result <- json
  spaces
  return result
  where 
    json = parseString ||| parseRational ||| parseObject ||| parseArray ||| parseTrue ||| parseFalse ||| parseNull
    parseString = jsonString >>= \a -> return (JsonString a)
    parseRational = jsonNumber >>= \a -> return (JsonRational True a)
    parseObject = jsonObject >>= \a -> return (JsonObject a)
    parseArray = jsonArray >>= \a -> return (JsonArray a)
    parseTrue = jsonTrue >>= const (return (JsonTrue))
    parseFalse = jsonFalse >>= const (return (JsonFalse))
    parseNull = jsonNull >>= const (return (JsonNull))

-- Exercise 9
-- Read a file into a JSON value.
-- ~~~ Use readFile and jsonValue ~~~
readJsonValue :: FilePath -> IO JsonValue
readJsonValue filePath = do
  s <- readFile filePath
  return (valueOr (jsonValue <.> s) JsonNull)
