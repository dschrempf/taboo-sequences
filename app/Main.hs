module Main where

import TabooSequence
import Data.List (zip4, intercalate)
-- import qualified Text.Show.Pretty as Pr

main :: IO ()
main = do
  let
    -- State spaces for taboo sequences.
    !ss = map stateSpace [1..23]
    -- State space sizes of taboo sequences.
    !cs = map length ss
    -- Average usage of Ones.
    !os = map totalAverage ss
    -- Average distance.
    !ds = map averageDistance ss
    -- Number of ones per sequence.
    !ops = map onesPerSequence ss
  prettyPrintAverages cs os ds
  putStrLn ""
  prettyPrintOnesPerSequence ops

-- | Pretty print state space sizes, average usage of Ones and average
-- distances.
prettyPrintAverages :: [Int] -> [Double] -> [Double] -> IO ()
prettyPrintAverages ss cs os = do
  putStrLn "Length StateSpaceSize Ones Distances"
  let ls = map formatLineAverages $ zip4 [1..] ss cs os
  putStrLn $ unlines ls

-- | Format a single line.
formatLineAverages :: (Int, Int, Double, Double) -> String
formatLineAverages (l, s, c, o) = unwords [show l, show s, show c, show o]

prettyPrintOnesPerSequence :: [[Int]] -> IO ()
prettyPrintOnesPerSequence os = do
  let l  = maximum (map length os)
      is = take l [0..] :: [Int]
  putStrLn "Number of sequences with X Ones."
  putStrLn $ formatLineOnesPerSequence is
  let ls = map formatLineOnesPerSequence os
  putStrLn $ unlines ls

formatLineOnesPerSequence :: [Int] -> String
formatLineOnesPerSequence = intercalate "\t" . map show
