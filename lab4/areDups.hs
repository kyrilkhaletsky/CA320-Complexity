areDups2 :: Eq a => [a] -> Bool
areDups2 [] = False
areDups2 (x:xs) = (x `elem` xs) || (areDups2 xs)
