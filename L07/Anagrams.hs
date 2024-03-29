module L07.Anagrams where

import Data.Char
import Data.List
--import Data.Function

{-
Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO String
* lines :: String -> [String]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-}

grabEm :: String -> [String] -> [String]
grabEm word words = map fst $ Prelude.filter f sdict
  where
    f item = sortedWord == (snd item) && word /= (fst item)
    sortedWord = sort word
    sdict = zip words (map sort words)

lowercase :: String -> String
lowercase = map toLower

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams :: String -> FilePath -> IO [String]
anagrams word dictionaryFile = do
    contents <- readFile dictionaryFile
    return $ grabEm (lowercase word) $ lines contents

-- Compare two strings for equality, ignoring case
equalIgnoringCase :: String -> String -> Bool
equalIgnoringCase s1 s2 = (lowercase s1) == (lowercase s2)
