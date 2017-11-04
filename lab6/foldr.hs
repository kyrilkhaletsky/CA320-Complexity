sumsquaresr :: Int -> Int
sumsquaresr n = foldr (\x y -> x^2+y) 0 [1..n]

lengthr :: [a] -> Int
lengthr xs = foldr (\x y -> y+1) 0 xs

reverser :: [a] -> [a]
reverser xs = foldr (\x ys -> ys++[x]) [] xs
