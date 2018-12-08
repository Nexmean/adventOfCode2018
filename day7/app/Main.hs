module Main where

import qualified Data.Set                      as Set
import           System.Environment
import           Text.Parsec
import           Text.Parsec.Char

main :: IO ()
main = do
  (filename : _) <- getArgs
  s              <- readFile filename
  let requirements = map parseRequirement $ lines s
  putStrLn $ solve1 requirements

type Requirement = (Char, Char)

requirementP :: Parsec String () Requirement
requirementP =
  (,)
    <$> (string "Step " *> anyChar <* string " must be finished before ")
    <*> (string "step " *> anyChar <* string " can begin.")

parseRequirement :: String -> Requirement
parseRequirement s = case parse requirementP "" s of
  Right requirement -> requirement

solve1 :: [Requirement] -> String
solve1 = placeChars

placeChars :: [Requirement] -> String
placeChars requirements = helper ""
 where
  helper s = if length s == length (placeChar requirements s)
    then s
    else helper $ placeChar requirements s

placeChar :: [Requirement] -> String -> String
placeChar requirements s =
  let charsCanPlace =
        Set.fromList $ filter canPlace
        $ filter (not . (`elem` s))
        $ concatMap (\(a, b) -> [a, b]) requirements
      canPlace c = all ((`elem` s) . fst) $ filter ((== c) . snd) requirements
  in  if null charsCanPlace then s else s ++ [minimum charsCanPlace]
