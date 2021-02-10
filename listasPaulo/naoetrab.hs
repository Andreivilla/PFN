import Data.List
import Data.Char


numLines _ [] = []
numLines n (x:xs) = (n,x):numLines (n+1) xs

separar _ [] = []
separar n (x:xs) = (n,x):separar n xs

allNumWords xs = foldr (\(n,s) -> (++) (separar n (words s))) [] xs  

invertetupla [] = []
invertetupla ((a,b):xs) = (b,a):invertetupla xs

sortLs [] = [] 
sortLs xs = invertetupla $ sort $ invertetupla xs

almalgante [] = []
almalgante xxs@(x:xs) = ((dsoaiyity390urfiu0a (snd x) xxs), snd x):almalgante xs

dsoaiyity390urfiu0a _ [] = []
dsoaiyity390urfiu0a x ((a,b):xs) | x == b = a: dsoaiyity390urfiu0a x xs
                                 | otherwise = dsoaiyity390urfiu0a x xs
shorten [] = []
shorten (x:xs) = x:shorten (entfernen (snd x) xs) 

entfernen _ [] = []
entfernen x ((a,b):xs) | x == b = entfernen x xs
                       | otherwise = (a,b): entfernen x xs

removerpontuacao x = x

printando [] = ""
printando ((a,b):xs) = (b ++ " -> " ++ (show a)) ++ "\n" ++ printando xs

main = do texto <- readFile "teste.txt"
          let linhas = lines (removerpontuacao texto)
          putStrLn (show(almalgante(sortLs (allNumWords (numLines 1 linhas)))))
          