module Main where

import           Data.List
import           Data.Traversable
import           System.Environment
import           Text.Parsec

import           Lib

main :: IO ()
main = do
  (filename : _) <- getArgs
  s              <- readFile filename
  let points = map parsePoint $ lines s
  putStrLn $ solve1 points
  print $ solve2 points

solve1 :: [Point] -> String
solve1 points =
  intercalate ['\n']
  $ flip map [top .. bottom] $ \y ->
    flip map [left .. right] $ \x ->
      if (x, y) `elem` positions then '#' else '.'
 where
  positions   = map _position finalPoints
  finalPoints = move points $ getSecond points
  top         = minimum $ map (snd . _position) finalPoints
  right       = maximum $ map (fst . _position) finalPoints
  bottom      = maximum $ map (snd . _position) finalPoints
  left        = minimum $ map (fst . _position) finalPoints

solve2 :: [Point] -> Int
solve2 = getSecond

getSecond :: [Point] -> Int
getSecond points = fst $ minimumOn snd $ map
  (\s -> (s, sumDistances (move points s)))
  [10500 .. 11000]
 where
  sumDistances points =
    sum [ distance (_position p1) (_position p2) | p1 <- points, p2 <- points ]

move :: [Point] -> Int -> [Point]
move points seconds = map movePoint points
 where
  movePoint (Point (x, y) (hv, vv)) =
    Point (x + hv * seconds, y + vv * seconds) (hv, vv)

distance :: Position -> Position -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = minimumBy (\a b -> f a `compare` f b)
