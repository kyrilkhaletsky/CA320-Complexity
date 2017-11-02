num :: Eq a => a -> [a] -> Int
num a [] = 0
num a (x:xs) | a == x = 1 + (num a xs)
   | otherwise = num a xs
