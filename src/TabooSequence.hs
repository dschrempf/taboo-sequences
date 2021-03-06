{- |
Description :  Taboo sequences
Copyright   :  (c) Dominik Schrempf 2018
License     :  GPL-3

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Mar 15 14:39:33 2018.

What are taboo sequences? How can we parse them? How can we manipulate them? All
of these questions are answered here.

-}


module TabooSequence
  ( Seq(..)
  , pSeq
  , valid
  , validPeriodic
  , stateSpace
  , stateSpacePeriodic
  , totalAverage
  , averageDistance
  , onesCountsPerSite
  , onesProbPerSite
  , onesPerSequence
  , flipSeq
  , connected
  , distance
  , getLength
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import qualified Data.Attoparsec.Text as A
import Data.List
import Data.Maybe
import TabooSequenceState
import Test.QuickCheck

-- | A taboo sequence is a list of binary sites.
newtype Seq = Seq { getSeq :: [State] } deriving (Eq)

instance Show Seq where
  show (Seq ss) = concatMap show ss

dropDoubleZeroes :: [State] -> [State]
dropDoubleZeroes []  = []
dropDoubleZeroes (Zero:Zero:xs) = dropDoubleZeroes $ Zero:xs
dropDoubleZeroes (Zero:One:xs)  = Zero : One : dropDoubleZeroes xs
dropDoubleZeroes (x:xs) = x : dropDoubleZeroes xs

instance Arbitrary Seq where
  arbitrary = Seq . dropDoubleZeroes <$> sized arbitrary' where
    arbitrary' n = replicateM n arbitrary

-- | A taboo sequence is any number of taboo sequence states.
pSeq :: A.Parser Seq
pSeq = many pState >>= checkValidity where
  checkValidity states | valid (Seq states)  = return $ Seq states
                       | otherwise = error "Seq contains two consecutive zeroes and is not valid."

-- | Is a taboo sequence valid?
valid :: Seq -> Bool
valid (Seq (x:y:xs))
  | (x == Zero) && (y == Zero) = False
  | otherwise                  = valid $ Seq (y:xs)
valid (Seq [_]) = True
valid _ = False

-- | Is a taboo sequence with periodic boundary condition valid?
validPeriodic :: Seq -> Bool
validPeriodic s@(Seq ss@(x:_)) = not headAndLastZero && valid s where
  headAndLastZero = all (== Zero) [x, last ss]
validPeriodic _ = False

-- | Check if lengths of given taboo sequences are equal and return length.
getLength :: [Seq] -> Maybe Int
getLength [] = error "No sequence given."
getLength ss = if length ls == 1 then Just (head ls) else Nothing where
  ls = nub $ map (length . getSeq) ss

-- | Get all taboo sequences of specific length.
stateSpace :: Int -> [Seq]
stateSpace n = filter valid $ map Seq $ stateSpace' n

-- | Get all taboo sequences of specific length and with periodic boundary conditions.
stateSpacePeriodic :: Int -> [Seq]
stateSpacePeriodic n = filter validPeriodic $ map Seq $ stateSpace' n

-- | Internal function. Get the state space of all binary sequences.
stateSpace' :: Int -> [[State]]
stateSpace' n | n <= 0    = [[]]
              | otherwise = (:) <$> [Zero, One] <*> stateSpace' (n-1)

totalAverage :: [Seq] -> Double
totalAverage ss = sum as / fromIntegral (length as) where
  as = map average ss

-- | Number of Ones divided by the length.
average :: Seq -> Double
average s@(Seq ss) = fromIntegral (ones s)  / fromIntegral (length ss)

-- | Number of Ones in a sequence.
ones :: Seq -> Int
ones (Seq ss) = length . filter (== One) $ ss

-- | Count the number of Ones per site.
onesCountsPerSite :: [Seq] -> [Int]
onesCountsPerSite seqS = [countOnes (nTh l) | l <- [0.. length (getSeq $ head seqS) - 1 ]] where
  ss = map getSeq seqS
  nTh n = map (!! n) ss
  countOnes = length . filter (== One)

-- | Probability of having a One per site.
onesProbPerSite :: [Seq] -> [Double]
onesProbPerSite ss = map ((/ fromIntegral n) . fromIntegral) (onesCountsPerSite ss) where
  n = length ss

-- | How many sequences have 0, 1, 2, ... Ones?
onesPerSequence :: [Seq] -> [Int]
onesPerSequence ss = foldl update zeroes nOnesList where
  nOnesList = map ones ss
  l = fromMaybe (error "Lengths vary.") (getLength ss)
  zeroes = replicate (l+1) (0 :: Int)
  update xs i = take i xs ++ [(xs !! i) + 1] ++ drop (i + 1) xs

-- | Average distance.
averageDistance :: [Seq] -> Double
averageDistance ss = 2*(1-a)*a where
  a = totalAverage ss

-- | Flip the ith state of a taboo sequence. Here it might be computationally be
-- better to check if flipping is possible before actually flipping.
flipSeq :: Int -> Seq -> Maybe Seq
flipSeq i (Seq ss) =
  if valid flippedSeq
  then Just flippedSeq
  else Nothing where
  flippedSeq = Seq $ ss & element i .~ flipState s
  s = ss !! i

connected' :: Int -> Seq -> Seq -> Bool
connected' 2 _      _      = False
connected' _ (Seq (_:_))  (Seq [])     = error "Sequences have different length."
connected' _ (Seq [])     (Seq (_:_))  = error "Sequences have different length."
connected' n (Seq (a:as)) (Seq (b:bs)) =
  if a == b
  then connected' n     (Seq as) (Seq bs)
  else connected' (n+1) (Seq as) (Seq bs)
connected' _ _ _ = True

connected :: Seq -> Seq -> Bool
connected = connected' 0

distance' :: Int -> Seq -> Seq -> Int
distance' n (Seq [])     (Seq [])     = n
distance' _ (Seq (_:_))  (Seq [])     = error "Sequences have different length."
distance' _ (Seq [])     (Seq (_:_))  = error "Sequences have different length."
distance' n (Seq (a:as)) (Seq (b:bs)) =
  if a == b
  then distance' n     (Seq as) (Seq bs)
  else distance' (n+1) (Seq as) (Seq bs)

distance :: Seq -> Seq -> Int
distance = distance' 0
