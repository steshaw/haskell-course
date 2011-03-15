module Main where

import L07.Anagrams

import System

main = do
  (word:dictionaryFilePath:[]) <- getArgs
  anagrams <- anagrams word dictionaryFilePath
  mapM_ putStrLn anagrams
