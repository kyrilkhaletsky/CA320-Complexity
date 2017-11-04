-- iter n f x applies function f n times with a starting value given by x

iter :: Int -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = f (iter (n-1) f x)

-- pow x n raises x to the power of n

pow :: Int -> Int -> Int
pow x n = iter n (*x) 1
