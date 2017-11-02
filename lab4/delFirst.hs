delFirst :: Eq a => a -> [a] -> [a]
delFirst _ [] = []
delFirst a (x:xs) | a == x = xs
   | otherwise = x : delFirst a xs
