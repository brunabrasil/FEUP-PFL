--3.1
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter f p l = map f (filter p l)

--3.2
dec2int :: Integral a => [a] -> a
dec2int l = foldl (\acc x -> acc * 10 +x) 0 l

-------------------------
--currying
func a b = (*a) b
--eta-redução
fund' a = (* a)
--eta-redução
func'' = (*)
---------------------------
--3.3

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f [] [] = []
myzipWith f (x:xs) (y:ys) = f x y : myzipWith xs ys

--3.4
--isort :: Ord a => [a] -> [a]

--conversão para point free
dec2int' :: Integral a => [a] -> a
dec2int' = foldl (\acc -> (+) (acc*10)) 0

dec2int'2 :: Integral a => [a] -> a
dec2int'2 = foldl (\acc -> ((+) . (+10)) acc) 0

dec2int'3 :: Integral a => [a] -> a
dec2int'3 = foldl ((+) . (+10)) 0

--3.7 c)
reverseRight :: [a] -> [a]
reverseRight l = foldr (\x acc -> acc ++ [x]) [] l
--d)
reverseLeft :: [a] -> [a]
reverseLeft l = foldl (\acc x -> x : acc ) [] l
--point free
reverseLeft' :: [a] -> [a]
reverseLeft' = foldl (flip (:)) []
