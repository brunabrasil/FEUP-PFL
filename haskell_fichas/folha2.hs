import Data.Char
--2.1
myand :: [Bool] -> Bool
myand [] = False
myand (x:xs)
  | xs /= [] = x && myand xs
  |otherwise = x

myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (h:t) = h ++ (concat' t)

concat'' :: [[a]] -> [a]
concat'' l =[x | xs <- l, x <- xs]

replicate' :: (Integral a) => a -> b -> [b]
replicate' 0 _ = []
replicate' n x
  | n > 0 = x: (replicate' (n-1) x)
  |otherwise = error ""

replicate'' :: (Integral a) => a -> b -> [b]
replicate'' n x = [x | _ <- [1..n]]

(@@) :: (Integral b) => [a] -> b -> a
[] @@ _ = error "lista vazia"
(x:_) @@ 0 = x
(_:xs) @@ n
  | n > 0 = xs @@ (n-1)
  | otherwise = error "argumento inteiro negativo"

(@@@) :: (Integral b) => [a] -> b -> a
l @@@ n = head [x | (x,i) <- zip l [0..], i==n]

reverse', reverse'' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

reverse'' l = reverse''Aux l []

reverse''Aux :: [a] -> [a] -> [a]
reverse''Aux [] acc = acc
reverse''Aux (x:xs) acc = reverse''Aux xs (x:acc)

elem' :: Eq a => a -> [a] -> Bool
elem' n l
  | null l = False
  | n == head l = True
  | otherwise = elem' n (drop 1 l)

--2.2

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [a] = [a]
intersperse n (x:xs) = x : n : (intersperse n xs)

--2.3
mdc :: Integer -> Integer -> Integer
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

--2.4
myinsert :: Ord a => a -> [a] -> [a]
myinsert a (x:xs)
  | a>x = x : (myinsert a xs)
  | a<x = a : x : xs

--b)
isort :: Ord a => [a] -> [a]
isort [] = []
isort [a] = [a]
isort (x:xs) = myinsert x (isort xs)

--2.5
--a)
minimum' :: Ord a => [a] -> a
minimum' [a] = a
minimum' (x:y:xs)
  | x <= y = minimum' (x:xs)
  | y <= x = minimum' (y:xs)

--b)
delete :: Eq a => a -> [a] -> [a]
delete n (x:xs)
  | n == x = xs
  | otherwise = x : (delete n xs)

--c)
ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort l = minimum' l : ssort (delete (minimum' l) l)

--2.6
soma :: Int -> Int
soma n = sum [x^2 | x <- [1 .. n]]

--2.7
aprox :: Int -> Double
aprox n = sum [((-1)^k) / fromIntegral (2*k+1) | k <- [0..n]]

--aprox' :: Int -> Double
--aprox'

--2.8
dotprod :: [Float] -> [Float] -> Float
dotprod x y =  sum [ a*b |(a,b) <- zip x y]

--2.9
divprop :: Integer -> [Integer]
divprop n = [ k | k <- [1..n-1], n `mod` k == 0]

--2.10
perfeitos :: Integer -> [Integer]
perfeitos n = [k | k <- [1..n-1], sum (divprop k) == k]

--2.11
pitagoricos :: Integer -> [(Integer,Integer,Integer)]
pitagoricos n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a^2+b^2==c^2]

--2.12
primo :: Integer -> Bool
primo n = (length (divprop n) +1) == 2

--2.13
mersennes :: [Int]
mersennes = [n | n <- [1..30], primo (2^n - 1)]

--2.14
binom :: Integer -> Integer -> Integer
binom n k = product [1 .. n] `div` (product [1 .. k] * product [1 .. (n - k)])

pascal :: Integer -> [[Integer]]
pascal n = [[binom y x| x <- [0..y]] | y <-[0..n]]

--2.15
cifrar :: Int -> String -> String
cifrar n [] = []
cifrar n (x:xs)
  | isUpper x = chr (((ord x - ord 'A') + n) `mod` 26 + ord 'A') : cifrar n xs
  | isLower x = chr (((ord x - ord 'a') + n) `mod` 26 + ord 'a') : cifrar n xs
  | otherwise = x : cifrar n xs

--2.16

myreplicate :: Int -> a -> [a]
myreplicate n a = [ a | _ <- [1..n]]

(??) :: [a] -> Int -> a
l ?? n = head [ a | (a,b) <- zip l [0..], b==n]
--2.17
forte :: String -> Bool
forte l = length l >= 8 && not (null ([ True | x <- l, isUpper x]))
  && not (null ([ True | x <- l, isLower x]))
  && not (null ([ True | x <- l, isDigit x]))

--2.18

--a)
mindiv :: Int -> Int
mindiv x = head [ q | p <- [floor(sqrt(fromIntegral x))..x], q <- [0..floor(sqrt(fromIntegral x))], x==p*q]

--b)
melhorprimo :: Int -> Bool
melhorprimo n = n > 1 && n == mindiv n

--2.19 ??
--nub :: Eq a => [a] -> [a]

--2.20
--transpose :: [[a]] -> [[a]]


--2.21
algarismos :: Int -> [Int]
algarismos n
  | n>0 = algarismos (n `div` 10) ++ [n `mod` 10]
  | otherwise = []

--2.22
toBits :: Int -> [Int]
toBits n
  | n > 0 = toBits (n `div` 2) ++ [n `mod` 2]
  | otherwise = []

--2.23
fromBits :: [Int] -> Int
fromBits [] = 0
fromBits (x:xs)
  | x == 1 = 2 ^ (length xs) + fromBits xs
  | otherwise = fromBits xs

--2.24
--a)
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge l [] = l
merge [] l = l
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | x > y = y : merge (x:xs) ys

--b)

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort l = merge (msort (take (length l `div` 2) l)) (msort (drop (length l `div` 2) l))
