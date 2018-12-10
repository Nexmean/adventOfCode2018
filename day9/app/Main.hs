module Main where

import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict                      as Map
import           System.Environment
-- import           Text.Parsec

main :: IO ()
main = do
  (players : marble : _) <- getArgs
  print $ solve1 (read players) (read marble)

solve1 :: Int -> Int -> Int
solve1 players marble = maximum $ Map.elems $ play players marble

type Player = Int
type Marble = Int

data GameState = GS { _marbles       :: [Marble]
                    , _currentMarble :: Marble
                    , _points        :: Map.Map Player Int
                    }

insertIn :: a -> Int -> [a] -> [a]
insertIn el 0 xs       = el : xs
insertIn el i (x : xs) = x : insertIn el (i - 1) xs

play :: Int -> Marble -> Map.Map Player Int
play playersCount lastMarble =
  let turns = zip (concat . repeat $ [0 .. playersCount - 1]) [0 .. lastMarble]
  in  _points
      $ (`execState` GS [] undefined Map.empty)
      $ forM turns
      $ \(player, marble) -> do
          ms <- gets _marbles
          cm <- gets _currentMarble
          if marble == 0 || marble `mod` 23 /= 0
            then do
              let (newCm, newMs) = commonTurn ms cm marble
              put =<< GS newMs newCm <$> gets _points
            else do
              let (newCm, newMs, addPoints) = specificTurn ms cm marble
                  alterHelper (Just points) = Just $ points + addPoints
                  alterHelper Nothing       = Just addPoints
              newPoints <- Map.alter alterHelper player <$> gets _points
              put $ GS newMs newCm newPoints

commonTurn :: [Marble] -> Marble -> Marble -> (Marble, [Marble])
commonTurn ms cm m =
  let cmIdx = case cm `elemIndex` ms of
        Just idx -> idx
      placeIndex = (cmIdx + 2) `mod` length ms
  in  if m /= 0 then (m, insertIn m placeIndex ms) else (0, [0])

specificTurn :: [Marble] -> Marble -> Marble -> (Marble, [Marble], Int)
specificTurn ms cm m =
  let cmIdx = case cm `elemIndex` ms of
        Just idx -> idx
      rmIdx    = if cmIdx - 7 >= 0 then cmIdx - 7 else length ms + (cmIdx - 7)
      newCmIdx = if rmIdx >= length ms - 1 then 0 else rmIdx
      newMs    = delete (ms !! rmIdx) ms
  in  (newMs !! newCmIdx, newMs, m + ms !! rmIdx)
