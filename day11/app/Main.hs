{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Applicative
import           Data.Foldable
import qualified Data.Map.Lazy                 as Map
import           Data.Traversable
import           Control.Monad.State
import           System.Environment

import           Lib

main :: IO ()
main = do
  (input : maxSize' : _) <- getArgs
  let serial  = read input
      maxSize = read maxSize'
  print $ solve1 serial
  print $ solve2 serial maxSize

type Serial = Int
type Position = (Int, Int)

solve1 :: Serial -> Position
solve1 serial = fst $ maximumOn snd blocks
 where
  rackID (x, _) = x + 10
  powerLevel p@(x, y) = hundreds ((rackID p * y + serial) * rackID p) - 5
  hundreds num = (num `div` 100) `mod` 10
  block (x, y) = [ (x', y') | x' <- [x .. x + 2], y' <- [y .. y + 2] ]
  blocks =
    [ ((x, y), sum $ map powerLevel $ block (x, y))
    | x <- [1 .. 298]
    , y <- [1 .. 298]
    ]

solve2 :: Serial -> Int -> Block
solve2 serial maximalSize =
  fst $ maximumOn snd $ (`evalState` Map.empty) $ mapM
    (\block -> (,) block <$> blockPowerLevel serial block)
    blocks
 where
  blocks =
    [ Block (x, y) size
    | x    <- [1 .. maximalSize]
    , y    <- [1 .. maximalSize]
    , size <- [1 .. min (maximalSize - x + 1) (maximalSize - y + 1)]
    ]

blockPowerLevel :: MonadState (Map.Map Block Int) m => Serial -> Block -> m Int
blockPowerLevel serial block = do
  v <- gets $ Map.lookup block
  case (v, block) of
    (Just powerLevel, _          ) -> pure powerLevel
    (Nothing        , Block pos 1) -> do
      let pl = powerLevel serial pos
      modify $ Map.insert block pl
      pure $ powerLevel serial pos
    (Nothing, block) -> do
      pl <- sum <$> mapM (blockPowerLevel serial) $ splitBlock block
      modify $ Map.insert block pl
      pure pl

splitBlock :: Block -> [Block]
splitBlock (Block pos 1) = [Block pos 1]
splitBlock (Block pos@(x, y) size)
  | size `mod` 2 == 0
  = let halfSize = size `div` 2
    in  [ Block pos                          halfSize
        , Block (x + halfSize, y)            halfSize
        , Block (x           , y + halfSize) halfSize
        , Block (x + halfSize, y + halfSize) halfSize
        ]
  | otherwise
  = [Block pos (size - 1)]
    ++ [ Block (x', y + size - 1) 1 | x' <- [x .. x + size - 1] ]
    ++ [ Block (x + size - 1, y') 1 | y' <- [y .. y + size - 2] ]

powerLevel :: Serial -> Position -> Int
powerLevel serial p@(x, y) =
  let rackID = x + 10
      hundreds num = (num `div` 100) `mod` 10
  in  hundreds ((rackID * y + serial) * rackID) - 5

maximumOn :: (Foldable f, Ord b) => (a -> b) -> f a -> a
maximumOn f = maximumBy (\a b -> f a `compare` f b)