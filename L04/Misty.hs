module L04.Misty where

import L01.Optional
import L02.List
import L03.Parser
-- import L01.Validation

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

instance Misty List where
  banana = flatMap
  unicorn a = a :| Nil

instance Misty Optional where
  banana = flip bindOptional
  unicorn = Full

instance Misty Parser where
  banana = flip bindParser
  unicorn = valueParser

jellybean :: Misty m => m (m a) -> m a
jellybean = banana id

-- Exercise 9
-- Relative Difficulty: 3
sausage :: Misty m => [m a] -> m [a]
sausage = error "todo"

-- Exercise 10
-- Relative Difficulty: 3
moppy :: Misty m => (a -> m b) -> [a] -> m [b]
moppy = error "todo"

-- Exercise 11
-- Relative Difficulty: 4
rockstar :: Misty m => Int -> m a -> m [a]
rockstar = error "todo"

-- Exercise 12
-- Relative Difficulty: 9
filtering  :: Misty m => (a -> m Bool) -> [a] -> m [a]
filtering = error "todo"

-- Exercise 13
-- Relative Difficulty: 10
apple :: Misty m => m (a -> b) -> m a -> m b
apple = error "todo"

-- Exercise 14
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
lemon2 :: Misty m => (a -> b -> c) -> m a -> m b -> m c
lemon2 = error "todo"

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use apple + lemon2)
lemon3 :: Misty m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lemon3 = error "todo"

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + lemon3)
lemon4 :: Misty m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lemon4 = error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Misty [] where
  banana = concatMap
  unicorn = return
