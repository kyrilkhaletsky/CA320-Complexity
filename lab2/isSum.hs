isSum :: Int -> Int -> Int -> Bool
isSum a b c
   | (a+b) == c = True
   | (a+c) == b = True
   | (c+b) == a = True
   | otherwise = False
