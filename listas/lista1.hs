--ex1--
concatena :: [a] -> [a] -> [a]
concatena [] ys = ys
concatena (x:xs) ys = x : concatena xs ys

--ex2--
pertence :: Eq a => a -> [a] -> Bool
pertence a [] = False 
pertence a (x:xs) = if a == x then True else pertence a xs

--ex3--
intersecao :: Eq a => [a] -> [a] -> [a]
intersecao [] _ = []
intersecao _ [] = []
intersecao (x:xs) ys = if pertence x ys then x:intersecao xs ys else intersecao xs ys

--ex4--
remove_last :: [a] -> [a]
remove_last [] = []
remove_last (x:xs) = if length xs == 0 then [] else x:remove_last xs

inverso :: [a] -> [a]
inverso [] = []
inverso xs = last xs:inverso (remove_last xs)

--ex5--
nprimeiros :: Int -> [a] -> [a]
nprimeiros 0 a = []
nprimeiros a [] = []
nprimeiros n (x:xs) = x:nprimeiros (n-1) xs

--ex6--
remove_init :: [a] -> [a]
remove_init [] = []
remove_init (x:xs) = xs

nultimos :: Int -> [a] -> [a]
nultimos 0 a = []
nultimos a [] = []
nultimos n xs = if length xs == n then xs else nultimos n (remove_init xs)

--ex7--
power :: Int -> Int -> Int 
power a 0 = 1
power a b = a*(power a (b-1))

strParaInt :: String -> Int
strParaInt [] = 0
strParaInt (x:xs)
    |x == '0' = aux*0 + binParaInt xs
    |x == '1' = aux*1 + binParaInt xs
    |x == '2' = aux*2 + binParaInt xs
    |x == '3' = aux*3 + binParaInt xs
    |x == '4' = aux*4 + binParaInt xs
    |x == '5' = aux*5 + binParaInt xs
    |x == '6' = aux*6 + binParaInt xs
    |x == '7' = aux*7 + binParaInt xs
    |x == '8' = aux*8 + binParaInt xs
    |x == '9' = aux*9 + binParaInt xs
    where aux = (power 10 (length xs))

charParaInt :: Char  -> Int 
charParaInt x
    |x == '0' = 0
    |x == '1' = 1
    |x == '2' = 2
    |x == '3' = 3
    |x == '4' = 4
    |x == '5' = 5
    |x == '6' = 6
    |x == '7' = 7
    |x == '8' = 8
    |x == '9' = 9

binParaInt :: String -> Int
binParaInt [] = 0
binParaInt (x:xs) = (charParaInt x)*(power 2 (length xs)) + binParaInt xs

--ex8--
intParaBin :: Int -> String
intParaBin 1 = ""
intParaBin n 
    |mod n 2 == 1 = '1':intParaBin (div n 2)
    |mod n 2 == 0 = '0':intParaBin (div n 2)

--ex9--
menorValor :: Ord a => [a] -> a
menorValor [x] = x
menorValor (x:xs) = if x<menorValor xs then x else menorValor xs

--ex10--
removerPrimeiro :: Eq a => [a] -> a -> [a]
removerPrimeiro (x:xs) n = if x == n then xs else x:removerPrimeiro xs n

--ex11--
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs = menorValor xs:ordenar (removerPrimeiro xs (menorValor xs))

--ex12--
dobrar_dir :: (a -> b -> b) -> b -> [a] -> b
dobrar_dir f a [] =  a
dobrar_dir f a (x:xs) = f x (dobrar_dir f a xs)

--ex13--
dobrar_esq :: (b -> a -> b) -> b -> [a] -> b
dobrar_esq f a [] = a
dobrar_esq f a (x:xs) = dobrar_esq f (f a x) xs

--ex14--
-- funcao para teste
par :: Integral a => a -> Bool
par n = if mod n 2 == 0 then True else False

--Escreva uma função que filtre uma lista, retornando os elementos que satisfazem um predicado:
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f [] = []
filtrar f (x:xs)
    |f x = x:filtrar f xs
    |otherwise = filtrar f xs

--ex15--
--funcao testar impar
impar :: Integral a => a -> Bool
impar a = if mod a 2 == 1 then True else False 

impares :: [Int] -> [Int]
impares [] = []
impares xs = filtrar impar xs

--ex16--
mapear :: (a -> b) -> [a] -> [b]
mapear f [] = []
mapear f (x:xs) = f x:mapear f xs

--ex17--
retirar (a,b) = a

primeiros :: [(a, b)] -> [a]
primeiros [] = [] 
primeiros xs = mapear retirar xs

--ex18--
todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = if x == True then todos xs else False


main :: IO ()
main = do
    print (concatena [1,2,3,4] [6,45,32])
    --print (pertence 8 [6,9,5,7,10])
    --print(intersecao [1,2,5,6,18] [18,3,9,10,1,5,6])
    --print(inverso [1,35,69,14,2])
    --print(nprimeiros 4 [1,2,5,6,18,3,9,10,1,5,6])
    --print(nultimos 4 [1,2,5,6,18,3,9,10,1,5,6])
    --print(strParaInt "0112654861")
    --print(binParaInt "1001010110")
    --print(intParaBin 45)
    --print(menorValor [4,5,0,8,9,1,65])
    --print(removerPrimeiro [1,5,8,9,32,14,65,98] 5)
    --print(ordenar [8,9,8,5,7,9,2,5,79,54])
    --print(remove_last [151,515,15,15,8948])
    --print(dobrar_dir (+) 0 [1..8])
    --print(dobrar_esq (+) 0 [1..8])
    --print(par 9)
    --print(filtrar par [2,5,8,9,56,4,65,59])
    --print(impar 8)
    --print(impares [2,5,8,9,56,4,65,59])
    --print(map (+7) [2,5,8,9,56,4,65,59])
    --print(primeiros [(1,2),(2,3),(4,5)])
    --print(todos [True, True, True, True])


