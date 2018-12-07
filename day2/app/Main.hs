module Main where

import           Control.Applicative
import           Control.Monad
import           System.Environment

main = do
  (filename : _) <- getArgs
  s              <- readFile filename
  print $ compute1 s
  traverse putStrLn $ compute2 s

compute1 s = with2char * with3char
 where
  with2char = length . filter char2 . lines $ s
  with3char = length . filter char3 . lines $ s

compute2 s =
  let commonLetters s1 s2 = map fst . filter (uncurry (==)) $ zip s1 s2
  in  do
        line1 <- lines s
        line2 <- lines s
        guard $ line1 /= line2
        guard $ diffBy1Char line1 line2
        pure $ commonLetters line1 line2

char2 s = 2 `elem` countChars s

char3 s = 3 `elem` countChars s

countChars s = do
  c <- s
  pure $ length . filter (== c) $ s

diffBy1Char s1 s2 = countDiffs s1 s2 == 1
  where countDiffs s1 s2 = length . filter (uncurry (/=)) $ zip s1 s2
