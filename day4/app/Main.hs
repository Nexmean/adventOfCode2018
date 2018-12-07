{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Applicative            ( liftA2 )
import           Control.Monad
import           Control.Monad.State
import           Data.Functor
import           Data.Maybe
import qualified Data.Map.Lazy                 as Map
import qualified Data.Set                      as Set
import           Data.Ord
import           Data.List
import           Data.Time
import           Data.Time.Clock
import           System.Environment
import           Text.Parsec

main :: IO ()
main = do
  (filename : _) <- getArgs
  s              <- readFile filename
  print $ solve1 s
  print $ solve2 s

-- TYPES
data LogRecord = LogRecord { time :: UTCTime
                           , action :: Action
                           } deriving (Eq, Show)

type GuardID = Int

data Action = BeginsShift GuardID | FallsAsleep | WakesUp
  deriving (Eq, Show)

maybeGuardID :: Action -> Maybe GuardID
maybeGuardID (BeginsShift g) = Just g
maybeGuardID _               = Nothing

data Shift = Shift { guard  :: GuardID
                   , events :: [LogRecord]
                   } deriving (Eq, Show)

-- PARSING
intP :: Parsec String () Int
intP = read <$> many1 digit

timeP :: Parsec String () UTCTime
timeP = between (char '[') (char ']') $ do
  (month, day) <- dateP
  space
  (hour, minute) <- timeP
  pure $ UTCTime
    (fromGregorian 2018 month day)
    (secondsToDiffTime . toInteger $ hour * 60 * 60 + minute * 60)
 where
  dateP = liftA2 (,) (intP *> char '-' *> intP) (char '-' *> intP)
  timeP = liftA2 (,) intP (char ':' *> intP)

actionP :: Parsec String () Action
actionP = space *> (beginsShift <|> fallsAsleep <|> wakesUp)
 where
  fallsAsleep = string "falls asleep" $> FallsAsleep
  wakesUp     = string "wakes up" $> WakesUp
  beginsShift =
    BeginsShift <$> (string "Guard #" *> intP <* string " begins shift")

logRecordP :: Parsec String () LogRecord
logRecordP = LogRecord <$> timeP <*> actionP

parseLogRecord :: String -> LogRecord
parseLogRecord s = case parse logRecordP "" s of
  Right record -> record

-- SOLVING FIRST PART
solve1 :: String -> Int
solve1 s = mostSleepingGuard * maxMinute
 where
  maxMinute = fst . maximumBy (\(_, ids1) (_, ids2) -> times ids1 `compare` times ids2)
    $ Map.assocs aggregated

  times = length . filter (== mostSleepingGuard)

  mostSleepingGuard = fst
    $ maximumBy (\(_, l1) (_, l2) -> l1 `compare` l2)
    $ map (\g -> (g, length . filter (== g) . concat $ Map.elems aggregated))
    $ Set.toList guardIDs

  guardIDs = Set.fromList (catMaybes $ map (maybeGuardID . action) records)

  aggregated = (\(_, _, m) -> m)
    $ execState (mapM_ aggregate records) (undefined, Awaken, Map.empty)

  records = sortOn time $ map parseLogRecord $ lines s

solve2 :: String -> Int
solve2 s = (\(m, g, _) -> m * g) $ maximumBy (\(_, _, i1) (_, _, i2) -> i1 `compare` i2) aggregated
 where

  guardIDs = Set.fromList (catMaybes $ map (maybeGuardID . action) records)

  aggregated :: [(Int, GuardID, Int)]
  aggregated = flatten $ Map.assocs
    $ fmap (Map.assocs . (flip execState Map.empty) . mapM_ aggregateIDsCount)
    $ (\(_, _, m) -> m)
    $ execState (mapM_ aggregate records) (undefined, Awaken, Map.empty)
   where
     flatten l = do
       (m, lg) <- l
       (g, i)  <- lg
       pure $ (m, g, i)

  records = sortOn time $ map parseLogRecord $ lines s

data ShiftState = SleepingFrom DiffTime | Awaken

aggregate
  :: MonadState (GuardID, ShiftState, Map.Map Int [GuardID]) m
  => LogRecord
  -> m ()
aggregate (LogRecord t (BeginsShift g)) = do
  (_, _, m) <- get
  put (g, Awaken, m)
aggregate (LogRecord t FallsAsleep) = do
  (g, _, m) <- get
  put (g, SleepingFrom $ utctDayTime t, m)
aggregate (LogRecord awaken WakesUp) = do
  (g, (SleepingFrom sleepingFrom), m) <- get
  let sleepingFromMinutes = toMinutes sleepingFrom
  let awakenMinutes       = toMinutes $ utctDayTime awaken
  let minutesToUpdate     = map fromInteger [sleepingFromMinutes .. awakenMinutes - 1]
  let alterMap = flip $ Map.alter $ \case
        Just guards -> Just $ g : guards
        Nothing     -> Just [g]
  let m' = foldl alterMap m minutesToUpdate
  put (g, Awaken, m')

aggregateIDsCount 
  :: MonadState (Map.Map GuardID Int) m
  => GuardID
  -> m ()
aggregateIDsCount g = modify $ Map.alter helper g
  where helper (Just i) = Just $ i + 1
        helper Nothing  = Just 1

toMinutes :: DiffTime -> Integer
toMinutes time =
  floor $ fromInteger (diffTimeToPicoseconds time) / (10 ^ 12 * 60)
