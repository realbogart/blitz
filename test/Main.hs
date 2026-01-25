module Main where

import Test.Hspec
import BlitzSpec qualified

main :: IO ()
main = hspec BlitzSpec.spec