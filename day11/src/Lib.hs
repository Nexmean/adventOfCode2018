module Lib where

data Block = Block { _position :: (Int, Int)
                   , _size     :: Int
                   } deriving (Eq, Ord, Show)