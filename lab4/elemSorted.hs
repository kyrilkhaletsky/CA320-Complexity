elemSorted :: Ord a => a -> [a] -> Bool
elemSorted a [] = False
elemSorted a (x:xs)
   | (a == x) = True
   | (a < x) = False
   | otherwise = elemSorted a xs
