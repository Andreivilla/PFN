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

binParaInt :: String -> Int
binParaInt "0" = 
    0
binParaInt "1" = 
    1
binParaInt ('0':xs) =
    binParaInt xs
binParaInt ('1':xs) =
    2 ^ length xs + binParaInt xs
binParaInt xs = 
    error "Valor não representa um numero binário!"


--ex8--
intParaBin :: Integer -> String 
intParaBin i =
    let divisao = i `div` 2 in
    if divisao > 0 then 
        if i `mod` 2 == 0 then
            concatena (intParaBin divisao) "0"
        else
            concatena (intParaBin divisao) "1"
    else
        if i `mod` 2 == 0 then
            "0"
        else
            "1" 

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
retirar :: (a, b) -> a
retirar (a,b) = a

primeiros :: [(a, b)] -> [a]
primeiros [] = [] 
primeiros xs = mapear retirar xs

--ex18--
todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = if x == True then todos xs else False

--Tree--
data Tree a = Leaf a | Branch a (Tree a) (Tree a)

--ex19--
ler :: Tree a -> [a]
ler (Leaf a) = [a]
ler (Branch a esq dir) = ler esq ++ [a] ++ ler dir

maiorValor :: Ord a => [a] -> a 
maiorValor [a] = a
maiorValor (x:xs) = if x>maiorValor xs then x else maiorValor xs

maior :: Ord a => Tree a -> a
maior a = maiorValor(ler(a))

--ex20--

altura :: Tree a -> Int
altura (Leaf a) = 1
altura (Branch a esq dir) = if altura esq > altura dir then (altura esq + 1) else (altura dir + 1)