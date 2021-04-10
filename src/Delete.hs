module Delete where
    import Control.Applicative

    safeDelete :: (Eq a) => [a] -> a -> Maybe [a]
    safeDelete [] _ = Nothing
    safeDelete (x:xs) y
        | x == y = return xs
        | otherwise = fmap (x:) (safeDelete xs y)
    
    safeDelete' :: (Eq a) => [a] -> [a] -> Maybe [a]
    safeDelete' x [] = Just x
    safeDelete' x (y:ys) = safeDelete x y >>= (`safeDelete'` ys )

        
    safeDelete'' :: (Eq a) => [a] -> [[a]] -> Maybe [a]
    safeDelete'' x [] = Just x
    safeDelete'' x (y:ys) = safeDelete' x y >>= (`safeDelete''` ys )