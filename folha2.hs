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

--2.6
sumSqr :: Int -> Int
sumSqr n = sum [x^2 | x <- [1 .. n]]

--2.8
--dotprod :: [Float] -> [Float] -> Float
--dotprod x y =  [ |zip x y]
