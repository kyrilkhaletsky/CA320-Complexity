shortest :: [[a]] -> [a]
shortest [] = []
shortest [y] = y
shortest (x:y:lst)
   | length x > length y = shortest (y:lst)
   | otherwise = shortest (x:lst)
