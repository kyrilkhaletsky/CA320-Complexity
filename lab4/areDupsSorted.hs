areDups :: Eq a => [a] -> Bool

areDups [] = False
areDups [x] = False
areDups (x:y:ys)
   | x==y      = True
   | otherwise = areDups (y:ys)
