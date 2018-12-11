{-# LANGUAGE TemplateHaskell #-}
module Lib where

import           Control.Applicative
import           Data.Maybe
import           Data.Label
import           Text.Parsec             hiding ( optional )

type Position = (Int, Int)
type Velocity = (Int, Int)

data Point    = Point { _position :: Position
                      , _velocity :: Velocity
                      }

mkLabel ''Point

pointP :: Parsec String () Point
pointP = Point <$> (spaces *> positionP) <*> (spaces *> velocityP)
 where
  positionP = string "position=" *> between
    (char '<')
    (char '>')
    ((,) <$> (spaces *> intP) <*> (char ',' *> spaces *> intP))
  velocityP = string "velocity=" *> between
    (char '<')
    (char '>')
    ((,) <$> (spaces *> intP) <*> (char ',' *> spaces *> intP))

intP :: Parsec String () Int
intP = do
  neg <- optional $ char '-'
  num <- read <$> many1 digit
  if isJust neg then pure $ negate num else pure num

parsePoint s = case parse pointP "" s of
  Right point -> point
  Left  err   -> error $ show err
