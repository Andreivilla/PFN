import Data.Char
import Data.List

-- 1. Usando a função filter declare uma lista infinita com os números naturais que são simultaneamente múltiplos de dois e três.

multiplos = filter (\n -> n `mod` 6 == 0) [1..]

-- 2. Declare uma função que recebe uma lista de Strings e retorna uma lista de duplas onde, o primeiro elemento é a String recebida, e e o segundo é o tamanho desta String.

contaletra [] = 0
contaletra (x:xs) = if isLetter x == True then 1 + contaletra xs else 0

contaPalavras [] = []
contaPalavras (xs:ys) = (xs, contaletra xs):contaPalavras ys

-- 3. Declare uma função que recebe um número n e retorna pares de valores distintos (x, y), tal que: 1<=x, y<=n. Utilize filter na construção.


-- 4. Declare uma função que recebe uma lista de caracteres, e retorne a mesma lista porém removendo as letras maiúsculas.

maiuscula [] = []
maiuscula (x:xs) = if isUpper x == True then maiuscula xs else x:maiuscula xs

semMaiusc [] = []
semMaiusc (xs:ys) = (maiuscula xs):semMaiusc ys

-- 5. Declare uma função que recebe uma lista de listas, e remove toda a ocorrência de lista vazia desta lista de listas.

semListasVazias [] = []
semListasVazias (xs:ys) = if null xs == True then semListasVazias ys else xs:semListasVazias ys

-- 6. Declare uma função que recebe uma lista com pares de inteiros e retorna outra lista que contém a soma de cada par.

somaPares [] = []
somaPares ((x,y):xs) = x+y:somaPares xs

-- 7.Declare uma função que recebe uma lista de números inteiros e retorna uma dupla de listas([a],[b]), onde [a] contempla os elementos ímpares, e [b] os elementos pares.

separaParImpar xs = ((filter odd xs),(filter even xs))

-- 8. Declare uma função com comportamento equivalente a função take da biblioteca prelude de Haskell, ou seja, deve receber um número inteiro n e uma lista, e retornar os n primeiros desta lista. Utilize as funções de ordem superior.


-- 9. Declare uma função, usando foldr, que receba um lista de valores booleanos e retorne a True se todos elementos da lista forem True, a função deve retornar False caso contrário.

foldbool [] = False
foldbool (x:xs) = foldr (==) True (x:xs)

-- 10. Declare uma função que recebe um número inteiro n, e retorna a soma do quadrado dos n primeiros números. Defina duas versões desta função, uma utilizando map e outra utilizando fold.

quadrado x = x*x

somaQuadrado n = sum (map (quadrado) [1..n])

somaQuadrado' n = foldr1 (+) (map (quadrado) [1..n])

-- 11. Declare a sua versão da função length disponível na biblioteca de Haskell Prelude, porém utilizando a função de ordem superior foldr.

length' (x:xs) = foldr (\_ -> (+1)) 0 (x:xs)

-- 12. Declare uma função que recebe uma lista de inteiros, e retorna o menor número inteiro desta lista. Defina 2 versões desta implementação, uma utilizando foldr e outra utilizando foldl.

minlista [] = 0
minlista (x:xs) = foldr (min) x (x:xs)

minlista' [] = 0
minlista' (x:xs) = foldl (min) x (x:xs)

-- 13. Declare uma função que recebe uma lista de Strings e retorna uma lista de booleanos tal que, True é o enésimo elemento da lista de inteiros se o enésimo elemento da lista de Strings tem um número de caracteres par, e False caso seja ímpar.

paridade [] = []
paridade (xs:ys) = if (mod (contaletra xs) 2) == 0 then True:paridade ys else False:paridade ys

-- 14. Declare uma lista infinita de números primos, use o crivo de Eratóstenes.