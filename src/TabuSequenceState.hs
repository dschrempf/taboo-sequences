{- |
   Description :  Tabu sequence states
   Copyright   :  (c) Dominik Schrempf 2017
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Creation date: Wed Feb  7 18:39:43 2018.

Definition of tabu sequence states and related functions.

-}


module TabuSequenceState
  ( State(..)
  , pState
  , flipState
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Attoparsec.Text
import Test.QuickCheck

-- | A single site is binary and can be either 0 or 1.
data State = Zero | One
  deriving (Eq)

-- | Use digits to represent the state.
instance Show State where
  show Zero = "0"
  show One  = "1"

instance Arbitrary State where
  arbitrary = do
    bool <- arbitrary :: Gen Bool
    if bool
      then return One
      else return Zero

-- | Parse a single state from a digit.
pState :: Parser State
pState = choice [char '0', char '1'] >>= check where
  check '0' = return Zero
  check '1' = return One
  check _   = error "State is binary, 0 or 1."


-- | Flip the tabu sequence state.
flipState :: State -> State
flipState Zero = One
flipState One  = Zero
