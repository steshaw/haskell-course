module L08.FastAnagrams where

--import Data.Char
import Data.List
--import Data.Function
import qualified Data.Set as S

import Steshaw ((|>))

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams :: String -> FilePath -> IO [String]
fastAnagrams word f = do
    contents <- readFile f
    return $ frobnicate contents
  where
    frobnicate dictionary = findEm (makeSet dictionary)
    findEm wordSet = filter ((flip S.member) wordSet) uniquePermutations
    makeSet dictionary = lines dictionary |> S.fromList
    uniquePermutations = permutations word |> S.fromList |> S.toList

{-
newtype NoCaseString =
  NoCaseString {
    ncString :: String
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
-}
