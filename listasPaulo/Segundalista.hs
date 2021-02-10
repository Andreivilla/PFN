-- 1. somaPares: escreva uma função que recebe um número e retorne a soma dos números pares entre 0 e ele mesmo.

somaPares 0 = 0
somaPares x = if mod x 2 == 0 then x + somaPares (x-1) else somaPares (x-1)

-- 2. somaQuadrado: escreva uma função que recebe dois parâmetros (m e n) e retorna aseguinte série: 2^1m + 2^2m + 2^3m+ ... + 2^nm.

somaquadrado x 0 = 0
somaquadrado x y = ((2^y)*x) + somaquadrado x (y-1)

somaquadrado' x 0 = 0
somaquadrado' x y = somaquadrado' x (y-1) + ((2^y)*x) 

--3. fibonacci: escreva 3 versões da função que recebe um número inteiro positivo e retorna o n-ésimo elemento da seqüência de Fibonacci. Uma versão deve usar casamento de padrões,uma versão deve usar a expressão if e uma versão deve usar guardas.

fib'' x 
       | x == 1 = 1
       | x == 0 = 0
       | otherwise = fib(x-1) + fib(x-2)

fib' 0 = 0
fib' x = if x == 1 then 1 else fib' (x-1) + fib' (x-2)

fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

-- 4. multiComSoma: escreva uma função que recebe dois números (fatores da multiplicação) e retorna seu produto. A função deve ser escrita utilizando apenas o operador de soma.

multiComSoma x 0 = 0
multiComSoma x y = x + multiComSoma x (y-1)

-- 5. primo: escreva uma função que recebe um número e retorne True caso ele seja primo e False, caso contrário. Um número primo é um número natural maior que 1, e que possui apenas dois divisores: 1 e ele mesmo.

primo' x 1 = 1
primo' x cont
             | rem x cont == 0 = 1 + primo' x (cont - 1)
             |otherwise = primo' x (cont - 1)


primo x = if (primo' x x > 2) then False else True