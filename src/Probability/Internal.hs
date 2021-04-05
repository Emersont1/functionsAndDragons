module Probability.Internal where
import Data.Bifunctor
import Control.Applicative

newtype Probability a = P [(a, Rational)]
strip (P x) = x

-- show
instance (Show a) => Show (Probability a) where
    show (P m) = show m

-- functor
instance Functor Probability where
    fmap f (P m) = P (fmap (first f) m)

-- applicative
instance Applicative Probability where
    pure x = P [(x, 1)]
    (P functions) <*> (P values) = P [(f x, p0*p1)
        | (x, p0)<-values, (f, p1) <- functions] 

-- monad
instance Monad Probability where
    (P values) >>= f = P [(v, p*p0) | (x, p0) <- values, (v,p) <- (strip . f) x ]

-- alternative
instance Alternative Probability where
    empty  = P []
    (P [] ) <|> r = r
    l <|> _ = l
