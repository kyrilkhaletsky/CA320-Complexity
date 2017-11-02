myAppend :: [a]->[a]->[a]
myAppend [] xs = xs
myAppend (y:ys) xs = y:(myAppend ys xs)

myHead :: [a] -> a
myHead [] = error "Called myHead with Empty List"
myHead (x:xs) = x

myLast :: [a] -> a
myLast [] = error "Called myLast with Empty List"
myLast [x] = x
myLast (x:xs) = myLast xs

myTail :: [a] -> [a]
myTail [] = error "Called myTail with Empty List"
myTail (x:xs) = xs

myInit :: [a] -> [a]
myInit [] = error "Called myInit with Empty List"
myInit [a] = []
myInit (x:xs) = x:(myInit xs)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ (myConcat xs)

mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

myProduct :: (Num a) => [a] -> a
myProduct [] = 0
myProduct [x] = x
myProduct (x:xs) = x * (myProduct xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "Called myMaximum with an Empty List"
myMaximum [x] = x
myMaximum (x:xs) = if x > (myMaximum xs)
                   then x
                   else myMaximum xs

myMinimum :: (Ord a) => [a] -> a
myMinimum [] = error "Called myMinimum with an Empty List"
myMinimum [x] = x
myMinimum (x:xs) = if x < (myMinimum xs)
                   then x
                   else myMinimum xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem y (x:xs) = if y == x
                  then True
                  else myElem y xs

myDelete :: (Eq a) => a -> [a] -> [a]
myDelete _ [] = []
myDelete y (x:xs) = if y == x
                    then xs
                    else x:(myDelete y xs)

myUnion :: (Eq a) => [a] -> [a] -> [a]
myUnion xs [] = xs
myUnion xs (y:ys) = if (myElem y xs) || (myElem y ys)
                    then myUnion xs ys
                    else myUnion (myAppend xs [y]) ys

myIntersect :: (Eq a) => [a] -> [a] -> [a]
myIntersect [] _ = []
myIntersect (x:xs) ys = if myElem x ys
                        then x:(myIntersect xs ys)
                        else myIntersect xs ys
