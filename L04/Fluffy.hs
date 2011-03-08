module L04.Fluffy where

import Prelude hiding (sum, length, map, filter, maximum, reverse)
import L01.Optional
import L03.Parser
import L02.List
-- import L01.Validation

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

instance Fluffy List where
  furry = map

instance Fluffy Optional where
  furry = mapOptional

instance Fluffy Parser where
  furry f a = bindParser a (valueParser . f)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fluffy [] where
  furry = fmap
