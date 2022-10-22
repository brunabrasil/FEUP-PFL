data Arv a = Vazia | No a ( Arv a) (Arv a) deriving Show
-- deriving faz com que a arvore seja  um tipo q pertence a class Show para mostrar em string

type Pair a b = (a,b)

myArv :: Arv Int
myArv = (No 3 (No 2 (No 1 Vazia Vazia ) Vazia) (No 4 Vazia Vazia))

sumArv :: (Num a) => Arv a -> a
sumArv Vazia = 0
sumArv (No x l r) = x + (sumArv l) + (sumArv r)

mapArv :: (a -> b) -> Arv a -> Arv b
mapArv _ Vazia = Vazia
mapArv f (No x l r) = No (f x) (mapArv f l) (mapArv f r)

-- Ex de listas infinitas no discord do prof

calcPi1 :: (Floating b, Enum b) => Int -> Int -> b
calcPi1 1 n = sum [ fromIntegral(4*(-1)^i) / fromIntegral(2*i + 1) | i <- [0..n-1]]

calcPi1 2 n = sum $ take n $ zipWith (/) (cycle [4,-4]) ([fromIntegral(2*i + 1) | i <- [0..]])

calcPi1 3 n = sum $ take n $ zipWith (/) (cycle [4,-4]) [1,3..]

calcPi2 :: (Floating b, Enum b) => Int -> Int -> b
calcPi2 1 n = 3 + sum [ fromIntegral(4*(-1)^i) / fromIntegral(product [2*i + 2..2*i + 4])| i <- [0..n - 2]]

calcPi2 2 n = sum $ take n $ zipWith (/) (cycle [4,-4]) ([fromIntegral(product [2*i + 2..2*i + 4])| i <- [0..]])

calcPi2 3 n = sum $ take n $ zipWith (/) (cycle [4,-4]) (zipWith (*) [2,4..] (zipWith (*) [3,5..] [4,6..]))

-----------------------
--4.7
inverteInput :: IO ()
inverteInput = do
  str <- getLine
  putStrLn $ reverse str

inverteInput' :: IO ()
inverteInput' = do
  str <- getLine
  if (null str) then
    return ()
  else do
    putStrLn $ reverse str
    inverteInput'

--4.9

elefantes :: Int -> IO ()
elefantes n = elefantesAux 2 n

elefantesAux :: Int -> Int -> IO ()
elefantesAux i n
  | (i < n) = do
    putStrLn $ "Se " ++ show i ++ " elefantes incomodam muita gente,"
    putStrLn $ show (i+1) ++ "incomodam muito mais!"
    elefantesAux (i+1) n
    | otherwise = return ()
