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
myzipWith f (x:xs) (y:ys) = f x y : myzipWith f xs ys

--3.4
--isort :: Ord a => [a] -> [a]

--3.5

maximum :: Ord a => [a] -> a
maximum l = foldl1 max l

--myFoldl1 f xs = foldl f (head xs) (tail xs)

mdc :: Int -> Int -> Int
mdc a b = fst (until (\(a,b) -> b == 0) (\(a,b) -> (b, a `mod` b)) (a,b))

----
--conversão para point free
dec2int' :: Integral a => [a] -> a
dec2int' = foldl (\acc -> (+) (acc*10)) 0

dec2int'2 :: Integral a => [a] -> a
dec2int'2 = foldl (\acc -> ((+) . (+10)) acc) 0

dec2int'3 :: Integral a => [a] -> a
dec2int'3 = foldl ((+) . (+10)) 0

--3.7
--a)
conc :: [a] -> [a] -> [a]
conc m n = foldr (:) m n

--b)
concat' :: [[a]] -> [a]
concat' l = foldr (\x acc -> x ++ acc) [] l

-- c)
reverseRight :: [a] -> [a]
reverseRight l = foldr (\x acc -> acc ++ [x]) [] l
--d)
reverseLeft :: [a] -> [a]
reverseLeft l = foldl (\acc x -> x : acc ) [] l
--point free
reverseLeft' :: [a] -> [a]
reverseLeft' = foldl (flip (:)) []

--e
elem' :: Eq a => a -> [a] -> Bool
elem' n l = any (==n) l

--3.8
palavras :: String -> [String]
palavras [] = []
palavras l = [takeWhile (/=' ') l] ++ palavras (dropWhile (==' ') (dropWhile (/=' ') l))

despalavras :: [String] -> String
despalavras l = tail (foldl (\acc x -> acc++" "++x) [] l)

--3.9
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' _ z [] = [z]
scanl' f z (x:xs) = f z x : scanl' f (f z x) xs
