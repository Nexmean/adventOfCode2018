module Main where

import           Control.Applicative
import           Control.Monad.State
import           Data.Foldable
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Maybe
import           Text.Parsec             hiding ( (<|>) )
import           System.Environment

main :: IO ()
main = do
  (filename : _) <- getArgs
  s              <- readFile filename
  let points = map parsePoint $ lines s
  print $ solve1 points
  print $ solve2 points

type Point = (Int, Int)
type Field = Point

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

intP :: Parsec String () Int
intP = read <$> many1 digit

pointP :: Parsec String () Point
pointP = liftA2 (,) (intP <* string ", ") intP

parsePoint :: String -> Point
parsePoint s = case parse pointP "" s of
  Right point -> point

solve1 :: [Point] -> Int
solve1 points =
  let
    (w, h) = (maximum (map fst points) + 1, maximum (map snd points) + 1)

    (fieldToPoint, pointToFieldsCount) = aggregate points

    infinite = Set.fromList $ mapMaybe (`Map.lookup` fieldToPoint) $ edges w h
  in
    maximum
    $ map snd
    $ filter (not . (`Set.member` infinite) . fst)
    $ Map.assocs pointToFieldsCount

aggregate :: [Point] -> (Map.Map Field Point, Map.Map Point Int)
aggregate points
  = let
      (w, h) = (maximum (map fst points) + 1, maximum (map snd points) + 1)

      (fieldToPoint, pointToFieldsCount) =
        (`execState` (Map.empty, Map.empty))
          $ forM [ (i, j) | i <- [0 .. w - 1], j <- [0 .. h - 1] ]
          $ \field -> do
              fieldToPoint       <- gets fst
              pointToFieldsCount <- gets snd

              let
                maps = fromMaybe (fieldToPoint, pointToFieldsCount) $ do
                  closest <- maybeMinimumOn (manhattanDistance field) points
                  let
                    fieldToPoint' = Map.insert field closest fieldToPoint

                    helper (Just i) = Just $ i + 1
                    helper Nothing  = Just 1

                    pointToFieldsCount' =
                      Map.alter helper closest pointToFieldsCount

                  pure (fieldToPoint', pointToFieldsCount')

              put maps
    in
      (fieldToPoint, pointToFieldsCount)

edges :: Int -> Int -> [Field]
edges w h =
  [ (i, j)
  | i <- [0 .. w - 1]
  , j <- [0 .. h - 1]
  , i == 0 || i == w - 1 || j == 0 || j == h - 1
  ]

maybeMinimumOn :: (Foldable f, Ord b) => (a -> b) -> f a -> Maybe a
maybeMinimumOn f cont =
  let min = minimumBy (\a b -> f a `compare` f b) cont
  in  if length (filter ((== f min) . f) (toList cont)) == 1
        then Just min
        else Nothing

solve2 :: [Point] -> Int
solve2 points =
  let (w, h) = (maximum (map fst points) + 1, maximum (map snd points) + 1)
      closeToPoints field =
        sum (map (manhattanDistance field) points) < maxTotalDistance
  in  length
      $ filter closeToPoints [ (i, j) | i <- [0 .. w - 1], j <- [0 .. h - 1] ]

maxTotalDistance :: Int
maxTotalDistance = 10000
