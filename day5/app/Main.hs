{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad.State
import           Data.Char
import           System.Environment

main :: IO ()
main = do
  (filename : _) <- getArgs
  s              <- readFile filename
  print $ length (solve1 s)

solve1 :: String -> String
solve1 s = if length s == length (reduce s) then s else solve1 $ reduce s

reduce = foldr helper ""
 where
  helper c [] = [c]
  helper c r  = if reducible c (head r) then tail r else c : r

  reducible a b =
    ((isLower a && isUpper b) || (isUpper a && isLower b))
      && toLower a
      == toLower b
