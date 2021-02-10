import Data.Char

{- 1. Declare uma função que receba uma lista de duplas [(a,b)], e retorne uma lista com o
primeiro elemento de cada dupla [a].-}

primeiros [] = []
primeiros ((a,b):xs) = a:primeiros xs

{- 2. Declare uma função que receba uma lista de duplas e retorne uma lista com a soma dos
elementos de cada dupla.-}

somarDuplas [] = []
somarDuplas ((a,b):xs) = (a+b):somarDuplas xs

{- 3. Declare uma função que receba uma lista e retorna uma dupla com o maior e o menor
elemento da lista.-}

menor [x] = x
menor (x:xs) = if x < menor xs then x else menor xs

maior [x] = x
maior (x:xs) = if x > maior xs then x else maior xs

maiorMenor [] = (0,0)
maiorMenor (x:xs) = (maior (x:xs), menor (x:xs))

{- 4. Declare uma função que receba um elemento e e uma lista de duplas, a função deve procurar
a primeira dupla cujo primeiro elemento da dupla seja igual ao parâmetro e. A função deve
retornar o segundo elemento dessa dupla.-}

procurar e [] = []
procurar e ((a,b):xs) = if e == a then b else procurar e xs

{- 5. Declare uma função que recebe uma lista de duplas e retorna uma lista contendo todas as
duplas cujo primeiro elemento seja menor que o segundo.-}


menores [] = []
menores ((a,b):xs) = if a < b then (a,b):menores xs else menores xs

{- 6. Declare uma função que receba como parâmetro uma String e retorne uma dupla de Strings,
a primeira String deve conter as letras maiúsculas e a segunda as letras minúsculas. Os
caracteres que não forem letras devem ser ignorados (Olhar as funções isLower e isUpper do
módulo Data.Char)-}

maius [] =[]
maius (x:xs) = if isUpper x == True then x:maius xs else maius xs

minus [] = []
minus (x:xs) = if isLower x == True then x:minus xs else minus xs 

maiuscMinusc [] = ([],[])
maiuscMinusc (x:xs) = ( maius (x:xs) , minus (x:xs))

{- 7. Declare uma função que receba um valor v e uma lista, a função deve retornar uma dupla de
listas, a primeira lista deve conter os elementos que são menores ou iguais a v e a segunda
lista deve retornar os elementos maiores que v.-}

menorV v [] = []
menorV v (x:xs) = if v >= x then x:menorV v xs else menorV v xs

maiorV v [] = []
maiorV v (x:xs) = if v < x then x:maiorV v xs else maiorV v xs

separar v [] = ([],[])
separar v (x:xs) = (menorV v (x:xs), maiorV v (x:xs))

{- 8. Implemente o Algoritmo de Euclides Estendido.-}

eMdc a 0 = (1, 0, a)
eMdc a b = (y, x - (div a b) * y, d)
     where 
         (x, y, d) = eMdc b (mod a b)

{- 9. Declare uma função que receba uma lista de duplas [(x,y)], e retorne uma lista com o inverso
de cada dupla, ou seja [(y,x)].-}

inverso [] = []
inverso ((a,b):xs) = (b,a):inverso xs

{- 10. Declare uma função que receba uma lista de duplas, e retorne lista indicando se os elementos
são iguais ou não (True/False).-}

simetrico [] = []
simetrico ((a,b):xs) = if a == b then True:simetrico xs else False:simetrico xs

{- 11. Declare uma função que recebe 2 listas de duplas [(a,b)] e [(c,d)], e retorna a composição da
primeira com a segunda lista, na forma [(a,d)], onde a é o primeiro elemento da primeira
lista, e c é o segundo elemento da segunda lista.-}

comporDuplas [] _ = []
comporDuplas _ [] = []
comporDuplas ((a,b):xs) ((c,d):ys) = (a,d):comporDuplas xs ys 

{- 12. Declare uma função que recebe um número inteiro, e retorna uma lista de duplas de inteiros
distintos (x,y) tal que 1 ≤ x, y ≤ i.-}


pares n = [(a,b) | a <- [1..n], b <- [1..n], a /= b]







