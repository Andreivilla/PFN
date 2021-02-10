-- Paulo Ricardo dos Reis

import Data.Char
import System.IO
import Data.List

main :: IO ()
main = do putStr "Arquivo: "
          hFlush stdout
          narq <- getLine
          texto <- readFile narq
          putStrLn (final(shorten(almalgamate(sortLs(inverte(allNumWords(numLines(lines(tirasimbolos texto)))))))))


-- a) Separar o documento em linhas: lines :: Doc → [Line]

-- lines :: String → [String] -- Divide um texto em linhas

-- b) Numerar as linhas do documento: numLines :: [Line] → [(Int,Word)]Line)]

numLines [] = []
numLines (x:xs) = zip [1..] (x:xs)

-- c) Associar a cada ocorrência de uma palavra do documento, o número da linha em que essa palavra ocorre: allNumWords :: [(Int,Word)]Line)] → [(Int,Word)]Word)]
-- words :: String → [String] -- Divide uma linha em palavras

allNumWords xs = foldr (\(n,s) -> (++) (separa_palavras n (words s))) [] xs

-- d) Ordenar alfabeticamente as ocorrências de palavras no texto: sortLs :: [(Int,Word)]Word)] → [(Int,Word)]Word)]

sortLs [] = []
sortLs xs = sort xs

-- e) Juntar as várias ocorrências de cada palavra, produzindo, para cada palavra, a lista dos números das linhas em que a palavra ocorre: almalgamate :: [(Int,Word)]Word)] → [([Int],Word)]Word)]

almalgamate [] = []
almalgamate (x:xs) = ((verifica (fst x) (x:xs)),fst x):almalgamate xs

-- f) Eliminar, da lista de números de páginas em que cada palavra ocorre, as repetições de um mesmo número de linha: shorten :: [([Int],Word)]Word)] → [([Int],Word)]Word)]

shorten [] = []
shorten (x:xs) = x:shorten (verifica' (snd x) xs)

-- Auxiliares

separa_palavras _ [] = []
separa_palavras n (x:xs) = (n,x):separa_palavras n xs

inverte [] = []
inverte ((n,x):xs) = (x,n):inverte xs

verifica _ [] = []
verifica x ((a,b):xs) = if x == a then b:verifica x xs else verifica x xs

verifica' _ [] = []
verifica' x ((a,b):xs) = if x == b then verifica' x xs else (a,b):verifica' x xs

tirasimbolos [] = []
tirasimbolos (x:xs)
                   |isSpace x == True = x:tirasimbolos xs
                   |isLetter x == False = tirasimbolos xs
                   |otherwise = x:tirasimbolos xs

final [] = []
final ((a,b):xs) = (b ++ ": " ++ (show a)) ++ "\n" ++ final xs



