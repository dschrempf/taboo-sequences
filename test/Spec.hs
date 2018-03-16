{-# LANGUAGE TemplateHaskell #-}
{- |
   Description :  Test my tabu sequences
   Copyright   :  (c) Dominik Schrempf 2017
   License     :  GPL-3

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  non-portable (not tested)

Creation date: Wed Feb  7 18:41:03 2018.

Some tests.

* Changelog

-}

import Control.Monad
import Control.Monad.Random
import TSState
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Transition

prop_valid :: Seq -> Bool
prop_valid = valid

prop_same_con :: Seq -> Bool
prop_same_con s = connected s s

prop_jump_once_connected :: Seq -> Property
prop_jump_once_connected s = monadicIO $ do
  s' <- run . evalRandIO $ jump s
  return $ connected s s'

prop_jump_twice_not_connected :: Seq -> Property
prop_jump_twice_not_connected s = monadicIO $ do
  s'  <- run . evalRandIO $ jump s
  s'' <- run . evalRandIO $ jump s'
  if s == s''
    then return True
    else return $ not (connected s s'')

prop_jump_n_max_distance :: Seq -> Property
prop_jump_n_max_distance s = monadicIO $ do
  n  <- run . evalRandIO $ getRandomR (2, 100)
  -- This is super complicated but should be easy. I want to repeatedly jump for
  -- n times. However, I have to prepare a list of monadic actions jumpM and
  -- then fold them using Kleisli compositions. WTF.
  let jumpM = run . evalRandIO . jump
  s' <- foldr (<=<) return (replicate n jumpM) s
  let d = distance s s'
  if d <= n
    then return True
    else return False

return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  putStrLn ""
  void runTests
