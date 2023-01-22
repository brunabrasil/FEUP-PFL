-- cap4

--HO-3
applyN :: Integral b => (a -> a) -> b -> a -> a
applyN _ 0 x = x
applyN f n x
  | n > 0 = f (applyN f (n-1) x)
  | otherwise = error "Negative number"

cipher :: Integral a => a -> String -> String
cipher _ [] = []
cipher n (x:xs) = (applyN nextChar n x) : cipher n xs

nextChar :: Char -> Char
nextChar 'z' = 'a'
nextChar x = succ x

--HO-7
--quick sort
sortByCond :: Ord a => [a] -> (a -> a -> Bool) -> [a]
sortByCond [] _ = []
sortByCond (x:xs) f = (sortByCond lower f) ++ (sortByCond upper f)
  where (lower, upper) = myPartititon x xs f

myPartititon :: Ord a => a -> [a] -> (a -> a -> Bool) -> ([a], [a])
myPartititon _ [] _ = ([],[])
myPartititon v (x:xs) f
  | f x v = (x:a, b)
  | otherwise = (a, x:b)
  where (a,b) = myPartititon  v xs f

--merge sort
sortByCond' :: Ord a => [a] -> (a -> a -> Bool) -> [a]
sortByCond' [] _ = []
sortByCond' [x] _ = [x]
sortByCond' l f = merge (sortByCond' l1 f) (sortByCond' l2 f) f
  where (l1,l2) = splitAt (div (length l ) 2) l

merge :: Ord a => [a] -> [a] -> (a -> a -> Bool) -> [a]
merge [] l _ = l
merge l [] _ = l
merge (x:xs) (y:ys) f
  | f x y = x:(merge xs (y:ys) f)
  | otherwise = y:(merge (x:xs) ys f)

--HO-8
myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f = (\x y -> f y x)

myUncurry :: (a -> b -> c) -> (a,b) -> c
myUncurry f (a,b) = f a b

--HO-14
orderedTriples :: Ord a => [(a,a,a)] -> [(a,a,a)]
orderedTriples = filter (\(a,b,c) -> (a <= b) && (b <= c))

--HO-15
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = (f x) : (myMap f xs)

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = [ x | x <- l, f x]

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile c (x:xs)
  | c x = x:(myTakeWhile c xs)
  | otherwise = []

--HO-18
myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f (x:xs)
  | f x = True && myAll f xs
  | otherwise = False

myAll' :: (a -> Bool) -> [a] -> Bool
myAll' f l = and (map f l)

--HO-22
countVowels :: String -> Int
countVowels l = length (filter ( \x -> (x == 'a') ||(x == 'e') || (x == 'i') ||  (x == 'o') || (x == 'u')) l)

--HO-32
myMap' :: (a -> b) -> [a] -> [b]
myMap' f l = foldr (\x acc -> (f x) : acc) [] l

largePairs ::  (Ord a, Num a) => a -> [(a,a)] -> [(a,a)]
largePairs m l = foldr (\(a,b) acc -> if (a + b > m) then (a,b):acc else acc) [] l

separateSingleDigits :: Integral a => [a] -> ([a], [a])
separateSingleDigits l = foldr (\x (xs,ys) -> if (x>= 0 && x <= 9) then (x:xs, ys) else (xs, x:ys)) ([], []) l

--HO-37
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

myFoldl ::  (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = myFoldl f (f acc x) xs

--HO-40 a
myScanr :: (a -> b -> b) -> b -> [a] -> [b]
myScanr f acc [] = [acc]
myScanr f acc (x:xs) = (f x $ head t):t
  where t = (myScanr f acc xs)

myScanr' :: (a -> b -> b) -> b -> [a] -> [b]
myScanr' f acc l = foldr (\x (y:ys) -> (f x y):y:ys) [acc] l

--HO-41
myScanl :: (b -> a -> b) -> b -> [a] -> [b]
myScanl _ acc [] = [acc]
myScanl f acc (x:xs) = acc:(myScanl f (f acc x) xs)

myScanl' :: (b -> a -> b) -> b -> [a] -> [b]
myScanl' f acc l = reverse (foldl (\(y:ys) x -> (f y x):y:ys ) [acc] l)

--HO-42
--myFoldl' :: (b -> a -> b) -> b -> [a] -> b
--myFoldl' f acc l = foldr (\x acc y -> a)

kadane :: (Ord a, Num a) => [a] -> a
kadane = maximum . scanl (\acc x -> max x (acc + x)) 0

--HO-47
f' :: [a] -> [a] -> [a]
f' xs ys = flip (foldr (:))

myLast :: [a] -> a
myLast = foldl1(flip const)

countLetters :: [Char] -> Int
countLetters = sum . map length words
