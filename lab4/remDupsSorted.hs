remDups :: Eq a => [a] -> [a]

remDups [] = []
remDups [x] = [x]
remDups (x:y:ys)
   | x==y      = remDups (y:ys)
   | otherwise = x:remDups (y:ys)
