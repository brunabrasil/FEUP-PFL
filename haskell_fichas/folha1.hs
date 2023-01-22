--folha 1 do moodle
--1.3
metades :: [a]->([a],[a])
metades l = (take (length l `div` 2) l, drop (length l `div` 2) l)

--1.4 a)
last' :: [a] -> a
last' l = head (reverse l)

last'' :: [a] -> a
last'' l = head (drop (length l - 1) l)

--b)
init' :: [a] -> [a]
init' l = reverse (tail (reverse l))

init'' :: [a] -> [a]
init'' l = take (length l - 1) l

--1.5 a)
binom :: Integer -> Integer -> Integer
binom n k = product [1 .. n] `div` (product [1 .. k] * product [1 .. (n - k)])

--b)
binom' :: Integer -> Integer -> Integer
binom' n k
 | k < n-k = product [(n-k+1) .. n] `div` product [1 .. k]
 | otherwise = product [(k+1) .. n] `div`  product [1 .. (n - k)]

--1.6

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c
  | delta >= 0 = ((-b + sqrt(delta)) / 2, (-b - sqrt(delta)) / 2)
  | otherwise = error "negative delta"
  where delta = (b^2) - 4*a*c

--1.8
--a)
segundo :: [a] -> a
segundo xs = head (tail xs)

--b)
trocar :: (a,b) -> (b,a)
trocar (x, y) = (y, x)

--c)
par :: a -> b -> (a,b)
par x y = (x, y)

--d)
dobro :: Num a => a -> a
dobro x = 2 * x

--e)
metade :: Fractional a => a -> a
metade x = x/2

--f)
minuscula :: Char -> Bool
minuscula x = x >= 'a' && x <= 'z'

--1.9
classifica :: Int -> String
classifica x
  | x <= 9 = "reprovado"
  | x <= 12 = "suficiente"
  | x <= 15 = "bom"
  | x <= 18 = "muito bom"
  | x <= 20 = "muito bom com distinção"
  | otherwise = "rip"

--1.11
max3, min3 :: Ord a => a -> a -> a -> a
max3 x y z = max (max x y) z
min3 x y z = min (min x y) z

--1.13
safetail :: [a] -> [a]
safetail [] = []
safetail (x:xs) = xs

--1.14
curta, curta' :: [a] -> Bool
curta l = length l <= 2
curta' [] = True
curta' [_] = True
curta' [_,_] = True
curta' _ = False

--1.15
mediana :: Ord a => a -> a -> a -> a
mediana x y z
  | x <= y && x >= z = x
  | x >= y && x <= z = x
  | y <= x && y >= z = y
  | y >= x && y <= z = y
  | z <= y && z >= x = z
  | z >= y && z <= x = z
  | otherwise = error "num tem"

mediana' :: (Num a, Ord a) => a -> a -> a -> a --why??
mediana' x y z = (x+y+z) - max3 x y z - min3 x y z

--1.16

converteUni :: Int -> String
converteUni x
  | x == 0 = "zero"
  | x == 1 = "um"
  | x == 2 = "dois"
  | x == 3 = "three"
  | x == 4 = "four"
  | x == 5 = "five"
  | x == 6 = "six"
  | x == 7 = "seven"
  | x == 8 = "eight"
  | x == 9 = "nine"

converteSpe :: Int -> String
converteSpe num
  | num == 10 = "dez"
  | num == 11 = "onze"
  | num == 12 = "doze"
  | num == 13 = "treze"
  | num == 14 = "catorze"
  | num == 15 = "quinze"
  | num == 16 = "dezasseis"
  | num == 17 = "dezassete"
  | num == 18 = "dezoito"
  | num == 19 = "dezanove"
  | otherwise = error "10sp not supposed"

converteDec :: Int -> String
converteDec num
  | num < 10 = converteUni num
  | num3 == 10 = converteSpe num
  | num == 20 = "vinte"
  | num3 == 20 = "vinte e " ++ converteUni num2
  where num2 = num mod 10



converte :: Int -> String
converte x
  | x < 0 = "menos " ++ converte (-num)
  | x < 10 = converteUni num
  | x < 100 = converteDec num
  | x < 1000 = converteCent num
