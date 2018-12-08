{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad.State
import           Data.Char
import           Data.Foldable
import           System.Environment

main :: IO ()
main = do
  (filename : _) <- getArgs
  s              <- readFile filename
  print $ solve1 s
  print $ solve2 s

solve1 :: String -> Int
solve1 = length . fullyReduce

solve2 :: String -> Int
solve2 s = minimum
  $ map (length . fullyReduce)
  $ map (\c -> filter (\c' -> c' /= toLower c && c' /= toUpper c) s) ['a' .. 'z']

fullyReduce :: String -> String
fullyReduce s = if length s == length (reduce s) then s else fullyReduce $ reduce s

reduce = foldr helper ""
 where
  helper c [] = [c]
  helper c s  = if reducible c (head s) then tail s else c : s

  reducible a b =
    ((isLower a && isUpper b) || (isUpper a && isLower b))
      && toLower a
      == toLower b
