module Probability(module Probability.Internal, dice, normalise, rand) where
    import Probability.Internal
    import Data.Bifunctor
    import Data.Ratio

    rand :: [a] -> Probability a
    rand  =  normalise . P . fmap (\x->(x,1))

    dice :: Integer -> Probability Integer
    dice n = P  . zip [1 .. n] . repeat $ 1%n

    normalise :: Probability a -> Probability a
    normalise (P x) = P (map (second (/s)) x)
        where s = foldl (\z (_, p)-> z + p) 0 x
