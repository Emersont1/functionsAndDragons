module Probability.Collapse(collapse, collapseEq, collapseSort) where
    import Probability.Internal
    import Data.List
    import Data.HashMap.Strict(empty, insertWith, toList)
    import Data.Hashable

    collapse :: (Eq a, Hashable a) => Probability a -> Probability a
    collapse (P x) = P (toList hm)
        where hm = foldl f empty x
              f m (v, p) = insertWith (+)  v p m

    collapseEq :: (Eq a) => Probability a -> Probability a
    collapseEq (P x) = P (foldl (\d (v,p) ->  seekAdd [] d v p) [] x) where
        seekAdd b [] v p1 = (v, p1):b
        seekAdd b ((c, p0):a) v p1
            | c == v = (c, p0+p1):b++a
            | otherwise = seekAdd ((c, p0):b) a v p1
    
    collapseSort :: (Ord a, Eq a) => Probability  a -> Probability a
    collapseSort (P x) = P . foldl' f [] . sortOn fst $ x where
        f [] v = [v]
        f ((v0, p0):xs) (v1, p1) 
            | v0 == v1 = (v0, p0+p1):xs 
            | otherwise = (v1, p1):(v0, p0):xs
        