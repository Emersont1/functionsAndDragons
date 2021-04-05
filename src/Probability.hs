module Probability(module Probability.Internal, dice, collapse, normalise) where
    import Probability.Internal
    import Data.Bifunctor
    import Data.Ratio

    dice :: Integer -> Probability Integer
    dice n = P  . zip [1 .. n] . repeat $ 1%n

    collapse :: (Eq a) => Probability a -> Probability a
    collapse (P x) = P (foldl (\d (v,p) ->  seekAdd [] d v p) [] x)

    seekAdd :: (Eq t1) => [(t1, Rational )] -> [(t1,  Rational )] -> t1 ->  Rational  -> [(t1,  Rational )]
    seekAdd b [] v p1 = (v, p1):b
    seekAdd b ((c, p0):a) v p1
        | c == v = (c, p0+p1):b++a
        | otherwise = seekAdd ((c, p0):b) a v p1
    
    normalise :: Probability a -> Probability a
    normalise (P x) = P (map (second (/s)) x)
        where s = foldl (\z (_, p)-> z + p) 0 x
