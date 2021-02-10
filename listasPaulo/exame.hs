-- Aluno: Paulo Ricardo dos Reis
import Data.Char
import Data.List

-- auxiliares 
maiores [] e = []
maiores (x:xs) e = if x > e then x:maiores xs e else maiores xs e
menores [] e = []
menores (x:xs) e = if x < e then x:menores xs e else menores xs e


--1.Declare uma função que receba como parâmetros uma lista e um dado d, a função deve retornar uma dupla de listas, a primeira lista deve conter todos os elementos menores que d e a segunda todos os elementos maiores.

maioresMenores (x:xs) e = ((menores (x:xs) e),(maiores (x:xs) e))

--2. Declare uma função que receba como parâmetros uma string e um caractere c, a função deve retornar a string recebida substituindo todas ocorrências do caractere c pelo caractere ‘*’.

marcar [] e = []
marcar (x:xs) e = if x == e then '*':marcar xs e else x:marcar xs e

--3. Declare uma função que receba como parâmetros um elemento e uma lista previamente ordenada, a função deve inserir esse elemento na lista tendo como resultado uma lista ordenada contendo o novo elemento

inserir e [] = [] 
inserir e (x:xs) = if e <= x then e:x:xs else x:inserir e xs

--4.Declare uma função que receba duas listas previamente ordenadas e faça a intercalação (merge) dos elementos tendo como resultado a junção das duas listas em uma lista também ordenada.

intercalacao xs ys = sortBy compare (xs++ys)

--5. Declare uma função que retorne o número de palavras em uma string.

contaPalavras [] = 1
contaPalavras (x:xs) = if isSpace x == True then 1 + contaPalavras xs else contaPalavras xs

--6. Declare a função main que solicite o nome de um arquivo texto, leia o arquivo e imprima quantas palavras e caracteres existem no arquivo


