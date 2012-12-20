{- |
Module      :  $Header$
Description :  Tests for the various ip address types in the system
Copyright   :  (c) Eric Merritt
License     :  Apache 2.0
-}
module Main (
  main
  ) where

import Test.Framework (TestOptions'(..),
                       RunnerOptions'(..), Test, defaultMainWithOpts)
import Data.Monoid (mempty)
import MulticastIP (multicastIPTests)

main :: IO ()
main = defaultMainWithOpts tests
       mempty {
         ropt_test_options =
            Just (mempty { topt_maximum_generated_tests = Just 1000 })
         }
tests :: [Test]
tests = concat [multicastIPTests]
