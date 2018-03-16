module Main where

import TabuSequence
import Data.List (zip4)
-- import qualified Text.Show.Pretty as Pr

main :: IO ()
main = do
  let
    -- State spaces for tabu sequences.
    !ss = map stateSpace [1..27]
    -- State space sizes of tabu sequences.
    !cs = map length ss
    -- Average usage of Ones.
    !os = map totalAverage ss
    -- Average distance.
    !ds = map averageDistance ss
  prettyPrint cs os ds

-- | Pretty print state space sizes, average usage of Ones and average
-- distances.
prettyPrint :: [Int] -> [Double] -> [Double] -> IO ()
prettyPrint ss cs os = do
  putStrLn "Length StateSpaceSize Ones Distances"
  let ls = map formatLine $ zip4 [1..] ss cs os
  putStrLn $ unlines ls

-- | Format a single line.
formatLine :: (Int, Int, Double, Double) -> String
formatLine (l, s, c, o) = unwords [show l, show s, show c, show o]
