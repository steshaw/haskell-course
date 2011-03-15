module Main where

import L08.FastAnagrams

import System

main = do
  (word:dictionaryFilePath:[]) <- getArgs
  anagrams <- fastAnagrams word dictionaryFilePath
  mapM_ putStrLn anagrams
