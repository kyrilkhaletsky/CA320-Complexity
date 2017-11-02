numSorted :: Ord a => a -> [a] -> Int
numSorted a [] = 0
numSorted a (x:xs) | (a == x) && (a <= x) = 1 + (numSorted a xs)
   | otherwise = numSorted a xs
