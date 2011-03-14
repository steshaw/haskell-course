module L06.MoreParser where

import Steshaw ((|>))
import L01.Validation
import L03.Parser

import Data.List
import Data.Char
import Numeric (readHex)
import Control.Applicative
import Control.Monad

-- Parses the given input and returns the result.
-- The remaining input is ignored.
(<.>) :: Parser a -> Input -> Validation a
(<.>) p = mapValidation snd . parse p

-- Exercise 1
-- Write a Functor instance for a Parser.
-- ~~~ Use bindParser and valueParser ~~~
instance Functor Parser where
  f `fmap` p = p `bindParser` \x -> valueParser (f x)

-- Exercise 2
-- Write an Applicative functor instance for a Parser.
-- ~~~ Use bindParser and valueParser ~~~
instance Applicative Parser where
  pure = valueParser
  pf <*> p1 = 
    pf `bindParser` \f -> 
    p1 `bindParser` \x ->
    valueParser (f x)

-- Exercise 3
-- Write a Monad instance for a Parser.
instance Monad Parser where
  return = valueParser
  (>>=) = bindParser

instance Monad Validation where
  return = valueValidation
  (>>=) = bindValidation

-- Exercise 4
-- Read documentation, ask questions.
{-

Check out the libraries now available to Parsers as a result of the Applicative and Monad instances.
4.1 http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Applicative.html
4.2 http://www.haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Control-Monad.html
4.3 http://haskell.org/ghc/docs/6.12.2/html/libraries/base-4.2.0.1/Data-Traversable.html

4.4 Learn about do-notation, now that we have just written a Monad instance
4.4.1 http://en.wikibooks.org/wiki/Haskell/do_Notation
4.4.2 http://www.haskell.org/onlinereport/exps.html#sect3.14

We will gently start using these libraries. Identify the pattern of computation in the problems below.

-}

-- Exercise 5
-- Write a parser that will parse zero or more spaces.
spaces :: Parser String
spaces = many0 space

-- Exercise 6
-- Write a function that applies the given parser, then parses 0 or more spaces,
-- then produces the result of the original parser.
-- ~~~ Use the monad instance ~~~
tok :: Parser a -> Parser a
tok p = do
  result <- p
  spaces
  return result

-- Exercise 7
-- Write a function that parses the given char followed by 0 or more spaces.
-- ~~~ Use tok and is ~~~
charTok :: Char -> Parser Char
charTok c = tok (is c)

-- Exercise 8
-- Write a parser that parses a comma ',' followed by 0 or more spaces.
-- ~~~ Use charTok ~~~
commaTok :: Parser Char
commaTok = charTok ','

-- Exercise 9
-- Write a parser that parses either a double-quote or a single-quote.
-- ~~~ Use is and (|||) ~~~
quote :: Parser Char
quote = is '\'' ||| is '"'

-- Exercise 10
-- Write a function that parses the given string (fails otherwise).
-- ~~~ Use is and mapM ~~~
string :: String -> Parser String
string s = mapM is s

-- Exercise 11
-- Write a function that parsers the given string, followed by 0 or more spaces.
-- ~~~ Use tok and string ~~~
stringTok :: String -> Parser String
stringTok s =  tok (string s)

-- Exercise 12
-- Write a function that tries the given parser, otherwise succeeds by producing the given value.
-- ~~~ Use (|||) ~~~
option :: a -> Parser a -> Parser a
option a p = p ||| return a

-- Exercise 13
-- Write a parser that parses 1 or more digits.
-- ~~~ Use many1 and digit ~~~
digits1 :: Parser String
digits1 = many1 digit

-- Exercise 14
-- Write a function that parses one of the characters in the given string.
-- ~~~ Use satisfy and elem ~~~
oneof :: String -> Parser Char
oneof s = satisfy (\c -> elem c s)

-- Exercise 15
-- Write a function that parses any character, but fails if it is in the given string.
-- ~~~ Use satisfy and notElem ~~~
noneof :: String -> Parser Char
noneof s = satisfy (\c -> notElem c s)

-- Exercise 16
-- Write a function that applies the first parser, runs the second parser keeping the result,
-- then runs the third parser and produces the obtained result.
-- ~~~ Use the Monad instance ~~~
between :: Parser o -> Parser c -> Parser a -> Parser a
between before after p = do
  before
  result <- p
  after
  return result

-- Exercise 17
-- Write a function that applies the given parser in between the two given characters.
-- ~~~ Use between and charTok ~~~
betweenCharTok :: Char -> Char -> Parser a -> Parser a
betweenCharTok c1 c2 p = do
  charTok c1
  result <- p
  charTok c2
  return result

-- Exercise 18
-- Write a function that parses the character 'u' followed by 4 hex digits and return the character value.
-- ~~~ Use readHex, isHexDigit, replicateM, satisfy and the Monad instance ~~~
hex :: Parser Char
hex = do
  is 'u'
  digits <- (replicateM 4 (satisfy isHexDigit))
  return ((readHex digits) |> head |> fst |> chr)

digits :: Parser Integer
digits = do
  digits <- many1 digit
  return (read digits)

-- Exercise 19
-- Write a function that produces a non-empty list of values coming off the given parser (which must succeed at least once),
-- separated by the second given parser.
-- ~~~ Use list and the Monad instance ~~~
sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 a sep = do
  first <- a
  rest <- many0 (sep >>> a)
  return (first:rest)

-- Exercise 20
-- Write a function that produces a list of values coming off the given parser,
-- separated by the second given parser.
-- ~~~ Use sepby1 and (|||) ~~~
sepby :: Parser a -> Parser s -> Parser [a]
sepby a s = sepby1 a s ||| return []

-- Exercise 21
-- Write a parser that asserts that there is no remaining input.
eof :: Parser ()
eof = P {
  parse = \input -> if null input then return (input, ()) else Error "expected EOF"
}

-- Exercise 22
-- Write a parser that produces a characer that satisfies all of the given predicates.
-- ~~~ Use sequence and Data.List.and ~~~
-- FIXME: Did not use Tony's recommended 'sequence'.
satisfyAll :: [Char -> Bool] -> Parser Char
satisfyAll preds = 
  character >>= \c ->
  if (Data.List.and $ preds <*> [c]) then return c else failed "did not satisfy all predicates"

-- Exercise 23
-- Write a parser that produces a characer that satisfies any of the given predicates.
-- ~~~ Use sequence and Data.List.or ~~~
-- FIXME: Did not use Tony's recommended 'sequence'.
satisfyAny :: [Char -> Bool] -> Parser Char
satisfyAny preds = 
  character >>= \c ->
  if (Data.List.or $ preds <*> [c]) then return c else failed "did not satisfy any predicates"

-- Exercise 24
-- Write a parser that parses between the two given characters, separated by a comma character ','.
-- ~~~ Use betweenCharTok, sepby and charTok ~~~
betweenSepbyComma :: Char -> Char -> Parser a -> Parser [a]
--betweenCharTok :: Char -> Char -> Parser a -> Parser a
betweenSepbyComma begChar endChar a = 
  betweenCharTok begChar endChar (sepby (tok a) commaTok)
