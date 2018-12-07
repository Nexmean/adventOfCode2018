{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Applicative
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import           Text.Parsec
import           System.Environment

main :: IO ()
main = do
  (filename : _) <- getArgs
  s              <- readFile filename
  print $ solve1 s
  print $ solve2 s

type ID = Int
type Position = (Int, Int)
type Size = (Int, Int)

type Fabric = Map.Map Position [ID]

data Claim = Claim ID Position Size

claimParser :: Parsec String () Claim
claimParser = do
  id       <- char '#' *> int
  _        <- space *> char '@' *> space
  position <- liftA2 (,) (int <* char ',') (int <* char ':')
  _        <- space
  size     <- liftA2 (,) (int <* char 'x') int
  pure $ Claim id position size

parseClaim :: String -> Claim
parseClaim s = case parse claimParser "" s of
  Right claim -> claim

int :: Parsec String () Int
int = read <$> many1 digit

claim :: Fabric -> Claim -> Fabric
claim f (Claim id (x, y) (w, h)) = foldl insertID f claimCells
 where
  claimCells = [ (i, j) | i <- [x .. x + w - 1], j <- [y .. y + h - 1] ]
  insertID   = flip $ Map.alter helper
   where
    helper mids = case mids of
      Just ids -> Just $ id : ids
      Nothing  -> Just [id]

solve1 :: String -> Int
solve1 =
  length
    . filter (> 1)
    . map length
    . Map.elems
    . foldl claim Map.empty
    . map parseClaim
    . lines

solve2 :: String -> ID
solve2 s = head $ filter (not . (`Set.member` overlappingIDs)) ids
 where
  claims = map parseClaim . lines $ s
  fabric = foldl claim Map.empty claims
  ids    = map (\(Claim id _ _) -> id) claims
  overlappingIDs =
    Set.fromList $ concat . filter ((> 1) . length) $ Map.elems fabric
