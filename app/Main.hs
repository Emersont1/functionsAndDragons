module Main where
import Data.List
import Probability
import Probability.Collapse
import Cards
import Control.Monad


cps :: (Eq a) => Probability a -> Probability a
cps = collapseEq

dmain :: IO ()
dmain = (print . normalise . cps ) (do x <- dice 6
                                       y <- dice 6
                                       let z = x + y
                                       guard (z /= 7)
                                       return z)

cmain :: IO ()
cmain =  (print . cps) (do shoe <- shuffleN 30
                           (c@(s,v), shoe) <- deal shoe
                           (c2@(s2,v2), shoe) <- deal shoe
                           return . sort $ [c, c2]
                           )
                 
                 

main = cmain