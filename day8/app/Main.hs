{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Data.Maybe
import           System.Environment
import           Text.Parsec

main :: IO ()
main = do
  (filename : _) <- getArgs
  s              <- readFile filename
  let numbers = map read $ words s
      root    = parseNode numbers
  print $ solve1 root
  print $ solve2 root

data Node = Node { _children :: [Node]
                 , _metadata :: [Int]
                 } deriving (Show)

solve1 :: Node -> Int
solve1 =
  let metasum (Node children meta) = sum meta + sum (map metasum children)
  in  metasum

solve2 :: Node -> Int
solve2 node =
  let
    value (Node []       meta) = sum meta
    value (Node children meta) = sum $ map value $ mapMaybe ((children !?) . flip (-) 1) meta

    []       !? _ = Nothing
    (x : _ ) !? 0 = Just x
    (x : xs) !? n = xs !? (n - 1)

  in value node

nodeP :: Parsec [Int] () Node
nodeP = do
  countChildren <- anyToken
  countMetadata <- anyToken
  children      <- count countChildren nodeP
  metadata      <- count countMetadata anyToken
  pure $ Node children metadata

parseNode :: [Int] -> Node
parseNode s = case parse nodeP "" s of
  Right node -> node
