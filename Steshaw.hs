{-# LANGUAGE RankNTypes #-}

module Steshaw
  (length
  ,(|>)
  ,($>)
  ,(//)
--  ,($$)
  ,(>.>)
  ,(>>>)
  ,unit
  ,d
  ,todo
  ,epsilon
  ,(%)
) where

import Prelude hiding (length)
import Data.List (genericLength)
import Control.Arrow ((>>>))

length :: [b] -> Integer
length = genericLength

type FlipCompose = forall b c. b -> (b -> c) -> c

(|>) :: FlipCompose
(|>) = flip ($)

($>) :: FlipCompose
($>) = flip ($)

{-
infixl 0 >$>
(>$>) = flip ($)
-}

(//) :: [a] -> (a -> b) -> [b]
(//) = flip map

-- Needed a ($) that binds a little less tightly to make it easy to use with (>$>).
{-
($$) = ($)
infixr 1 $$
-}

-- NOTE: Probably just use Control.Arrow's (>>>) as an alternative.
-- Interestingly, Control.Arrow(>>>) is made infixr 1.
(>.>) :: (a->b) -> (b -> c) -> (a -> c)
(>.>) = flip (.)
infixl 9 >.>

-- Alternative name for monadic return.
unit :: (Monad m) => a -> m a
unit = return

--
-- Attempt to simulate something similar to Ruby's number literal syntax. e.g. 260_000
--
-- e.g. d[260,000]
--
d :: (Num a) => [a] -> a
d = foldl (\ acc i -> acc * 1000 + i) 0

todo :: String -> t
todo functionName = error $ "Implement " ++ functionName

epsilon :: Double
epsilon = 1 $> iterate (/2) $> map (+1) $> takeWhile (/= 1) $> last $> (-) 1

(%) :: Fractional a => a -> a
(%) n = n / 100.0

{-
I don't know how established it is, but ($>) is a common name for flip($). Common, as in, I've seen three or so different people use it. IIRC, the F# name is (|>) but triangles are far too nice of a name and are often used for other things (e.g., cons and snoc). For what it's worth, this is the T combinator--- in case thrushes give you any ideas for names. Personally I'm more likely to use T as a prefix combinator rather than as infix, and for that the ($ _) section is good enough for me.

As for flip(.) we have (>>>) in Control.Arrow.
-}
