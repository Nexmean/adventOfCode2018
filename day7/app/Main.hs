{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Monad.Loops
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable
import           Data.List
import qualified Data.Set                      as Set
import           System.Environment
import           Text.Parsec
import           Text.Parsec.Char

main :: IO ()
main = do
  (filename : _) <- getArgs
  s              <- readFile filename
  let requirements = map parseRequirement $ lines s
  putStrLn $ solve1 requirements
  print $ solve2 requirements

type Requirement = (Char, Char)

requirementP :: Parsec String () Requirement
requirementP =
  (,)
    <$> (string "Step " *> anyChar <* string " must be finished before ")
    <*> (string "step " *> anyChar <* string " can begin.")

parseRequirement :: String -> Requirement
parseRequirement s = case parse requirementP "" s of
  Right requirement -> requirement

solve1 :: [Requirement] -> String
solve1 = placeChars

placeChars :: [Requirement] -> String
placeChars requirements = helper ""
 where
  helper s = if length s == length (placeChar requirements s)
    then s
    else helper $ placeChar requirements s

placeChar :: [Requirement] -> String -> String
placeChar requirements s = if null (charsCanPlace requirements s)
  then s
  else s ++ [minimum (charsCanPlace requirements s)]

type Worker = (Char, Int)

data WorkersState = WState { _second :: Int
                           , _placed :: String
                           , _workers :: [Worker]
                           }

solve2 :: [Requirement] -> Int
solve2 requirements =
  let workersCount = 5
      inWork workers c = c `elem` map fst workers

      step = do
        WState second placed workers <- get
        let afterStep     = map (bimap id (flip (-) 1)) workers
            busyWorkers   = filter ((> 0) . snd) afterStep
            completeChars = map fst $ filter ((<= 0) . snd) afterStep
        put $ WState (second + 1) (placed ++ completeChars) busyWorkers

      countFree workers = workersCount - length workers
  in  _second
      $ (`execState` WState 0 "" [])
      $ whileM
          (not <$> (isDone requirements <$> gets _placed <*> gets _workers))
      $ do
          WState second placed workers <- get
          when (countFree workers /= 0) $ do
            let newWorkers =
                  map (\c -> (c, charCost c))
                    $ take (countFree workers)
                    $ filter (not . inWork workers)
                    $ toList
                    $ charsCanPlace requirements placed
            put $ WState second placed (workers ++ newWorkers)
          step


isDone :: [Requirement] -> String -> [Worker] -> Bool
isDone requirements placed workers =
  null workers && null (charsCanPlace requirements placed)

charCost :: Char -> Int
charCost c = ord c - (ord 'A' - 1) + 60

charsCanPlace :: [Requirement] -> String -> Set.Set Char
charsCanPlace requirements placed =
  Set.fromList
    $ filter (canPlace requirements placed)
    $ filter (not . (`elem` placed))
    $ concatMap (\(a, b) -> [a, b]) requirements

canPlace :: [Requirement] -> String -> Char -> Bool
canPlace requirements placed c =
  all ((`elem` placed) . fst) $ filter ((== c) . snd) requirements
