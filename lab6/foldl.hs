sumsquaresl :: Int -> Int
sumsquaresl n = foldl (\x y -> y^2+x) 0 [1..n]

lengthl :: [a] -> Int
lengthl xs = foldl (\x y -> x+1) 0 xs

reversel :: [a] -> [a]
reversel xs = foldl (\ys x -> x:ys) [] xs
