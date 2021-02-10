import Data.Char

-- 1. Declare uma função que verifica se um elemento pertente a uma lista, a função deve retornar a True se o elemento estiver na lista e False caso contrário.

pertence x [] = False
pertence x (y:ys) = if x == y then True else pertence x ys

-- 2. Declare uma função que retorne a intercessão entre duas listas.

intercessao [] _ = []
intercessao _ [] = []
intercessao (x:xs) (y:ys) = if x == y then x:intercessao xs ys else intercessao xs ys

-- 3. Declare uma função que retorne o inverso de uma lista.

inverso [] = []
inverso (x:xs) = inverso xs ++ [x]

-- 4. Declare uma função que retorne os n últimos elementos de uma lista.

nprimeiros n (x:xs) = if n > 0 then x:nprimeiros (n-1) xs else []

nUltimos n (x:xs) = inverso(nprimeiros n (inverso (x:xs)))

-- 5. Declare uma função que receba duas listas de números e crie uma lista com a soma do primeiro elemento da primeira lista com o primeiro elemento da segunda lista, a soma do segundo elemento da primeira lista com o segundo elemento da segunda lista e assim sucessivamenteaté que uma das listas termine.

soma2 [] _ = []
soma2 _ [] = []
soma2 (x:xs) (y:ys) = (x+y):soma2 xs ys 

-- 6. Declare uma função que receba como parâmetro um número n e retorne uma lista com todas as potências de 2 até 2n

pot2' 0 = [1]
pot2' 1 = [2]
pot2' n = 2^n:pot2' (n-1)
pot2 n = inverso(pot2' n)

-- 7. Declare uma função que receba duas listas previamente ordenadas e faça a intercalação (merge) dos elementos tendo como resultado a junção das duas listas em uma lista também ordenada.

intercalacao [] _ = []
intercalacao _ [] = []
intercalacao (x:xs) (y:ys) = if x < y then x:intercalacao xs (y:ys) else y:intercalacao (x:xs) ys

-- 8. Declare uma função que retorne o menor valor de uma lista.

menor [x] = x
menor (x:xs) = if x < menor xs then x else menor xs

-- 9. Declare uma função que receba uma lista e um elemento e retorne a lista sem a primeira ocorrência desse elemento.

removerElem _ [] = [] 
removerElem n (x:xs) = if n == x then xs else x:removerElem n xs

-- 10. Usando as declarações anteriores, declare uma função que ordene os elementos de uma lista.

ordenar [] = []
ordenar (x:xs) = menor (x:xs):ordenar (removerElem (menor (x:xs)) (x:xs))

-- 11. Declare uma função que receba um elemento é uma lista ordenada insira este elemento na lista colocando-o na posição correta, ou seja, a lista resultante deve estar ordenada. Se o elemento já pertencer à lista, ele não deve ser incluído.

ins n (x:xs) | n < x = n:(x:xs)
             | n > x = x:ins n xs
             | n == x = xs 

-- 12. Declare uma função que receba um número n e uma lista e retorne o n-ésimo elemento da lista.

enesimo _ [] = 0
enesimo n (x:xs) = if n == 1 then x else enesimo (n-1) xs

-- 13. Declare uma função que receba um inteiro n e um elemento e e crie uma lista com n elementos e.

repetir n e = if n > 0 then e:repetir (n-1) e else []

-- 14. Declare uma função que converta um inteiro em uma String.

digito 0 = '0'
digito 1 = '1'
digito 2 = '2'
digito 3 = '3'
digito 4 = '4'
digito 5 = '5'
digito 6 = '6'
digito 7 = '7'
digito 8 = '8'
digito 9 = '9'

numString x = digito x

-- 15. Declare uma função que converta uma String contendo uma sequência de dígitos para um inteiro.

stringNum x = inverso(stringNum' x)

stringNum' 0 = []
stringNum' x = digito(rem x 10):stringNum' (div x 10)

-- 16. Declare uma função que receba uma String e converta todas letras maiúsculas dessa String em letras minúsculas.

minusculas [] = []
minusculas (x:xs) = if (isLower x) == True then x:minusculas xs else (toLower x):minusculas xs










