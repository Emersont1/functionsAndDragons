module Dice where
    import Probability
    d :: Int -> Int -> Probability Int
    d 1 n = dice n
    d m n = do ds <- d (m-1) n
               d' <- dice n
               return $ ds + d'
    
    p :: Probability Int -> Int -> Probability Int
    p n x= fmap (+x) n