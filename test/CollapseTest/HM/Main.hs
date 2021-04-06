import Data.List
import Probability
import Probability.Collapse
import Cards
import Control.Monad
import Data.Hashable


cps :: (Eq a, Hashable a) => Probability a -> Probability a
cps = collapse

main :: IO ()
main =  (print . cps) (do shoe <- shuffleN 30
                          (c@(s,v), shoe) <- deal shoe
                          (c2@(s2,v2), shoe) <- deal shoe
                          return . sort $ [c, c2]
                          )