--Learning Haskell by Solving Problems
-- week 2
fib :: (Num a, Ord a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib x
  | x > 0 = fib (x-2) + fib (x-1)
  | otherwise = error "negative"

ackermann :: (Num a, Ord a, Num t, Ord t) => a -> t -> t
ackermann m n
  | m == 0 = n+1
  | m > 0 && n == 0 = ackermann (m-1) 1
  | m > 0 && n > 0 = ackermann (m-1) (ackermann m (n-1))
  | otherwise = error "negative"

scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct [] [] = 0
scalarProduct (x:xs) (y:ys) = x*y + (scalarProduct xs ys)

seq22 :: Num a => Int -> [a]
seq22 n = [1] ++ [2 | _ <- [1..(n-2)]] ++ [1]

seq42 :: Num a => Int -> [a]
seq42 n = 1:(take (n-2) (cycle [4,2])) ++ [1]

myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup [x] = [[x]]
myGroup (x:y:xs)
  | x==y = (x:g):gs
  | otherwise = [x]:g:gs
  where (g:gs) = myGroup (y:xs)

-- ????
myInits :: [a] -> [[a]]
myInits (x:xs) = [] : (addInits x (myInits xs))

addInits :: a -> [[a]] -> [[a]]
addInits _ [] = []
addInits y (z:zs) = (y:z):(addInits y zs)
