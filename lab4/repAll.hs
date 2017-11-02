repAll :: Eq a => a -> a -> [a] -> [a]
repAll a b [] = []
repAll a b (x:xs) 
   | (x == a) = b : repAll a b xs
   | otherwise = x : repAll a b xs
