{- |
   Description :  Transition from one taboo sequence to another
   Copyright   :  (c) Dominik Schrempf 2017
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Creation date: Wed Feb  7 18:08:15 2018.

Function to move between taboo sequences.

* Changelog

-}

module Transition where

import TabooSequence
import Control.Monad.Random.Strict

-- | Move from one sequence to another.
jump :: MonadRandom m => Seq -> m Seq
jump ts@(Seq tss) = do
  let l = length tss
      flipLoop = do
        i <- getRandomR (0, l-1)
        let flipped = flipSeq i ts
        case flipped of
          Nothing    -> flipLoop
          Just value -> return value
  flipLoop
