sumPoly :: [Int] -> [Int] -> [Int]
sumPoly as [] = as
sumPoly [] bs = bs
sumPoly (a: as) (b: bs) = a + b : sumPoly as bs

evalPoly :: Int -> [Int] -> Int
evalPoly a [] = 0
evalPoly b (a:as) = a + b * (evalPoly b as)


