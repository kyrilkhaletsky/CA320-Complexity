isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs 
   | length xs < 2 = True
   | otherwise = (head xs == last xs) && (isPalindrome (init(tail xs)))



