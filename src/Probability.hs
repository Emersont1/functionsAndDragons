module Probability(module Probability.Internal, dice, normalise) where
    import Probability.Internal
    import Data.Bifunctor
    import Data.Ratio

    dice :: Int -> Probability Int
    dice n = P  . zip [1 .. n] . repeat $ 1%fromIntegral n

    normalise :: Probability a -> Probability a
    normalise (P x) = P (map (second (/s)) x)
        where s = foldl (\z (_, p)-> z + p) 0 x
