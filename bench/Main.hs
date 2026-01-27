module Main where

import Blitz qualified
import Criterion.Main

-- Example benchmarks for common operations
main :: IO ()
main =
  defaultMain
    [ bgroup
        "String operations"
        [ bench "reverse short string" $ whnf reverse "hello",
          bench "concatenation" $ whnf (++ "world") "hello "
        ]
    ]
