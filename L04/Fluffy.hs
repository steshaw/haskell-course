module L04.Fluffy where

import Prelude hiding (sum, length, map, filter, maximum, reverse)
import L01.Optional
import L03.Parser
import L02.List
{-
import L01.Validation
-}

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy List where
   --furry = L02.List.map
   furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Optional where
    --furry = mapOptional
    furry = L01.Optional.mapOptional

-- Exercise 3
-- Relative Difficulty: 2
instance Fluffy Parser where
  furry f a = bindParser a (valueParser . f)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fluffy [] where
  furry = fmap
