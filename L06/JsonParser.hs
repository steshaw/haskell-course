module L06.JsonParser where

import Prelude hiding (exponent)
import L03.Parser
import L06.JsonValue
import L06.MoreParser

jsonNull
  :: Parser JsonValue
jsonNull =
  do stringThenSpaces "null"
     return JsonNull

jsonTrue
  :: Parser JsonValue
jsonTrue =
  do stringThenSpaces "true"
     return JsonNull

jsonFalse
  :: Parser JsonValue
jsonFalse =
  do stringThenSpaces "false"
     return JsonNull

fractional
  :: Parser String
fractional =
  do is '.'
     d <- digits
     return ('.':d)

exponent ::
  Parser String
exponent =
  do e <- oneof "eE"
     s <- option '+' (oneof "+-")
     d <- digits
     return (e:s:d)

int ::
  Parser String
int =
  do s <- option [] (string "-")
     v <- string "0" ||| digits
     return (s ++ v)

number ::
  Parser JsonValue
number =
  do i <- int
     f <- option [] fractional
     e <- option [] exponent
     return . JsonNumber . read . concat $ [i,f,e]
