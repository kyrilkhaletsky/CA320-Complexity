sumPoly :: [Int] -> [Int] -> [Int]
sumPoly as [] = as
sumPoly [] bs = bs
sumPoly (a: as) (b: bs) = a + b : sumPoly as bs

