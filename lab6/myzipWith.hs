myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith _ _ _ = []
myzipWith f (x:xs) (y:ys) = (f x y):(myzipWith f xs ys)
