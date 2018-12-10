module Main where

import           Control.Applicative
import           Control.Monad.State
import           Data.Maybe
import           Data.List
import qualified Data.List.PointedList         as PL
import qualified Data.Map.Lazy                 as Map
import           System.Environment
import           Text.Parsec             hiding ( (<|>)
                                                , many
                                                )

main :: IO ()
main = do
  (players : marble : _) <- getArgs
  print $ solve1 (read players) (read marble)

solve1 :: Int -> Int -> Int
solve1 players marble = maximum $ Map.elems $ play players (Marble marble)

type Player = Int
newtype Marble = Marble { getMarble :: Int } deriving (Eq, Show)

type Circle = PL.PointedList Marble

data GameState = GS { _circle :: Circle
                    , _points :: Map.Map Player Int
                    }

play :: Int -> Marble -> Map.Map Player Int
play playersCount lastMarble =
  let turns = zip (concat . repeat $ [0 .. playersCount - 1])
                  (map Marble [0 .. getMarble lastMarble])
  in  _points
      $ (`execState` GS undefined Map.empty)
      $ forM turns
      $ \(player, marble) -> do
          circle <- gets _circle
          if getMarble marble == 0 || getMarble marble `mod` 23 /= 0
            then do
              let newCircle = commonTurn circle marble
              put =<< GS newCircle <$> gets _points
            else do
              let (newCircle, addPoints) = specificTurn circle marble
                  alterHelper (Just points) = Just $ points + addPoints
                  alterHelper Nothing       = Just addPoints
              newPoints <- Map.alter alterHelper player <$> gets _points
              put $ GS newCircle newPoints

commonTurn :: Circle -> Marble -> Circle
commonTurn circle marble = if marble == Marble 0
  then PL.singleton marble
  else
    fromMaybe undefined
    $   PL.insert marble
    <$> (PL.moveN 1 circle <|> scrolledCircle)
 where
  scrolledCircle =
    PL.moveTo ((PL.index circle + 1) `mod` PL.length circle) circle

specificTurn :: Circle -> Marble -> (Circle, Int)
specificTurn circle (Marble points) = (reduced, points + addPoint)
 where
  reduced = fromMaybe undefined $ PL.delete moved
  moved =
    fromMaybe undefined $ PL.moveN (-7) circle <|> PL.moveTo scrollN circle
  scrollN           = PL.length circle + (PL.index circle - 7)
  (Marble addPoint) = PL._focus moved
