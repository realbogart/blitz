module Main where

import BlitzSpec qualified
import Test.Hspec

main :: IO ()
main = hspec BlitzSpec.spec
