module Main where

import Probability
import Probability.Collapse
import Cards
import Control.Monad


dmain :: IO ()
dmain = (print . normalise . collapse ) (do x <- dice 6
                                            y <- dice 6
                                            let z = x + y
                                            guard (z /= 7)
                                            return z)

cmain :: IO ()
cmain =  (print . collapseEq) (do shoe <- shuffle
                                  (s,v) <- deal
                                  (s2,v2) <- deal
                                  if s /= Clubs then return s
                                  else return s2
                               )
                 
                 

main = cmain 