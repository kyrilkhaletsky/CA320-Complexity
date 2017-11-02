repFirst :: Eq a => a -> a -> [a] -> [a]
repFirst a b [] = []
repFirst a b (x:xs) 
   | (x == a) = b:xs
   | otherwise = x : repFirst a b xs


