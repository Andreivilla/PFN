import Data.Char

-- Aluno : Paulo Ricardo dos Reis

{-
1.

a) fib 6 = 8

Processo recursivo:
 
fib 6 = fib(6-1)+fib(6-2)
fib 6 = fib(5)+fib(4)
fib 6 = 5 + 3 = 8
fib 5 = fib(5-1)+fib(5-2)
fib 5 = fib(4)+fib(3)
fib 5 = 3 + 2 = 5
fib 4 = fib(4-1)+fib(4-2)
fib 4 = fib(3)+fib(2)
fib 4 = 2 + 1 = 3
fib 3 = fib(3-1)+fib(3-2)
fib 3 = fib(2)+fib(1)
fib 3 = 1 + 1 = 2
fib 2 = fib(2-1)+ fib(2-2)
fib 2 = fib(1)+ fib(0)
fib 2 = 1 + 0 = 1
fib 1 = 1
fib 0 = 0

b) len xs = foldr (\x y -> 1 + y) 0 xs

len [2,8,5,10,28] = foldr (\x y -> 1 + y) 0 xs
len [2,8,5,10,28] = foldr (\x y -> 1 + y) 0 [2,8,5,10,28]
len [2,8,5,10,28] = foldr (\x 0 -> 1 + 0) 0 [2,8,5,10,28]
len [2,8,5,10,28] = foldr (\x y -> 1 + y) 1 [2,8,5,10]
len [2,8,5,10,28] = foldr (\x 1 -> 1 + 1) 1 [2,8,5,10]
len [2,8,5,10,28] = foldr (\x y -> 1 + y) 2 [2,8,5]
len [2,8,5,10,28] = foldr (\x 2 -> 1 + 2) 2 [2,8,5]
len [2,8,5,10,28] = foldr (\x y -> 1 + y) 3 [2,8]
len [2,8,5,10,28] = foldr (\x 3 -> 1 + 3) 3 [2,8]
len [2,8,5,10,28] = foldr (\x y -> 1 + y) 4 [2]
len [2,8,5,10,28] = foldr (\x 4 -> 1 + 4) 4 [2]
len [2,8,5,10,28] = foldr (\x y -> 1 + y) 5 []

-}

-- 2.Definir uma função que recebe duas strings como parâmetro, e remove da segunda lista as letras que ocorrerem na primeira lista.

stringsemrep _ [] = []
stringsemrep [] _ = []
stringsemrep (x:xs) (y:ys) = if (pertence y (x:xs)) == False then y:stringsemrep xs ys else stringsemrep xs ys

--auxiliar
pertence x [] = False
pertence x (y:ys) = if x == y then True else pertence x ys

-- 3. Definir uma função que recebe uma string, e retorna uma lista de todos as substrings possíveis que formam um prefixo da string

substrings [] = []
substrings (xs) = (xs):substrings (inverso(removerElem(inverso xs)))

--auxiliares
inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

removerElem [] = [] 
removerElem (x:xs) = xs

-- 4.Definir uma função que recebe uma lista e um número inteiro, que indica o número de trocas de posições. Uma troca envolve capturar o primeiro elemento da lista e adicioná-lo ao fim da lista.

trocafim n (x:xs) = if n <= 0 then (x:xs) else trocafim (n-1)(xs ++ [x])

-- 5. Definir uma função que recebe uma lista de duplas, e retorne uma lista os elementos reposicionados.Reposicionar a lista envolve capturar o segundo elemento da cabeça da lista e o primeiro elemento da dupla seguinte, enquanto seja possível.

segundoprimeiro ((c,d):[])=[]
segundoprimeiro ((a,b):(c,d):xs) = (snd(a,b), fst(c,d)):segundoprimeiro ((c,d):xs)

-- 6.Definir a função zip3, que recebe 3 listas [a] -> [b] -> [c], e retorna uma lista de tuplas de tamanho 3 [(a,b,c)]. 

zip3' _ _ [] = []
zip3' _ [] _ = []
zip3' [] _ _ = []
zip3' (x:xs)(y:ys)(z:zs) = (x,y,z):zip3(xs)(ys)(zs)

-- 7. Definir uma função que recebe uma String, avalie cada letra e retorne True caso seja vogal, e False caso contrário. Devem ser removidos os espaços em branco antes. Utilizar função de ordem superior na implementação.

ehvogal [] = []
ehvogal (x:xs) = map (vogal)(removeEspaco (x:xs))

--auxiliares
vogal x = if x == 'a' || x == 'e' || x == 'i'|| x == 'o' || x == 'u' then True else False

removeEspaco [] = []
removeEspaco (x:xs) = if (isSpace x == True) then removeEspaco xs else x:removeEspaco xs

-- 8.Definir uma função que recebe uma lista de strings, e inverta cada elemento da lista, e então inverta a lista resultante. Não deve ser utiliza a função reverse do Prelude.

inversoExtInt (x:xs) = inverso(inversoExtIntcomeco (x:xs))

--auxiliar 
inversoExtIntcomeco [] = []
inversoExtIntcomeco ((x:xs):ys) = inverso (x:xs):inversoExtIntcomeco(ys)











