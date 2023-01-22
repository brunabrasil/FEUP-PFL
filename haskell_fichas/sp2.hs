--Learning Haskell by Solving Problems
-- week 2

--IN-17
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

--FT-14
scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct [] [] = 0
scalarProduct (x:xs) (y:ys) = x*y + (scalarProduct xs ys)

--FT-18
seq22 :: Num a => Int -> [a]
seq22 n = [1] ++ [2 | _ <- [1..(n-2)]] ++ [1]

seq42 :: Num a => Int -> [a]
seq42 n = 1:(take (n-2) (cycle [4,2])) ++ [1]

--LI-13
mySplitAt :: Int -> [a] -> ([a],[a])
mySplitAt 0 l = ([],l)
mySplitAt _ [] = ([], [])
mySplitAt n (x:xs)
  | n > 0 = (let (a,b) = mySplitAt (n-1) xs in (x:a,b))
  | otherwise = error "negative number in first argument"

--LI-14
myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup [x] = [[x]]
myGroup (x:y:xs)
  | x == y = (x:g):gs
  | otherwise = [x]:g:gs
  where (g:gs) = myGroup (y:xs)

-- ????
myInits :: [a] -> [[a]]
myInits [] = [[]]
myInits (x:xs) = [] : (addInits x (myInits xs))

addInits :: a -> [[a]] -> [[a]]
addInits _ [] = []
addInits y (z:zs) = (y:z):(addInits y zs)

-- LI-16

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = [(x,y)] ++ myZip xs ys

myZip3 :: [a] -> [b] -> [c]-> [(a,b,c)]
myZip3 [] _ _= []
myZip3 _ [] _ = []
myZip3 _ _ [] = []
myZip3 (x:xs) (y:ys) (z:zs) = [(x,y,z)] ++ myZip3 xs ys zs

--LI-17
differentFromNext :: Eq a => [a] -> [a]
differentFromNext [a] = []
differentFromNext [] = []
differentFromNext (x:y:xs)
  | x /= y = [x] ++ (differentFromNext (y:xs))
  | otherwise = differentFromNext (y:xs)

--LI-18 ?????
myTranspose :: [[a]] -> [[a]]
myTranspose [] = [[]]
myTranspose m = hs:(myTranspose ts)
  where (hs,ts) = splitHeadsTails m

splitHeadsTails :: [[a]] -> ([a], [[a]])
splitHeadsTails [] = ([], [[]])
splitHeadsTails (ys:xs) =
  case ys of [] -> (hs, ts)
            [z] -> (z:hs, ts)
            (z:zs) -> (z:hs, zs:ts)
  where (hs,ts) = splitHeadsTails xs

--LI-31
differentFromNext' :: Eq a => [a] -> [a]
differentFromNext' l = [ x | (x,y) <- zip l (tail l), x /= y]

--LI-32
conseqPairs :: Eq a => [a] -> [(a,a)]
conseqPairs l = [ (x,y) | (x,y) <- zip l (tail l) ]

myZip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
myZip3' a b c = [(x,y,z)| (x, (y,z)) <- zip a (zip b c)]

checkMod3ThenOdd :: [Integral] -> Bool
checkMod3ThenOdd l = and[mod x 2 == 1| x <- l, mod x 3 == 0]

repeatNTimes :: Integral -> [a] -> [a]
repeatNTimes n l = [ x | x <- l, _ <- [1..n]]

--LI-39!! sai no exame talvezz
myPermutations :: Eq a => [a] -> [[a]]
myPermutations [] = [[]]
myPermutations l = [ x:y | x <- l, y <- myPermutations (delete x l)]
