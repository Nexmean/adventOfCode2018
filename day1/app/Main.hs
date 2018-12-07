{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Data.Function
import           Control.Applicative
import           Control.Monad.State
import           System.Environment

main = do
  (filename : _) <- getArgs
  s              <- readFile filename
  print $ last . frequencies . changes $ s
  print $ firstTwice . changes $ s

parseLine :: String -> (Integer -> Integer)
parseLine s = op num
 where
  op  = if head s == '+' then (+) else flip (-)
  num = read $ tail s

changes :: String -> [Integer -> Integer]
changes = map parseLine . lines

frequencies :: [Integer -> Integer] -> [Integer]
frequencies = scanl (&) 0

firstTwice :: [Integer -> Integer] -> Integer
firstTwice changes = evalState (inner $ frequencies $ repeatList changes) []
 where
  repeatList = concat . repeat
  inner (x : xs) = do
    wasBefore <- gets (elem x)
    if wasBefore
      then pure x
      else do
        modify (x :)
        inner xs
