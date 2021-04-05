module Main where

import Probability
import Control.Monad


main :: IO ()
main = (print . normalise . collapse ) (do x <- dice 6
                                           y <- dice 6
                                           let z = x + y
                                           guard (z /= 7)
                                           return z)
