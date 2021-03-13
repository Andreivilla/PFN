ultimoElemento :: [a] -> a
ultimoElemento [x] = x
ultimoElemento (x:xs) = ultimoElemento xs
--lembrando q a função last faz isso tambem sendo assim a funcão tambem poderia ser feita da seguinte forma
--ultimoElemento :: [a] -> a
--ultimoElemento a = last a

lista :: [Int]
lista = 1:lista

maiorElemento :: Ord a => [a] -> a
maiorElemento [x] = x
maiorElemento (x:xs) = if x>maiorElemento xs then x else maiorElemento xs

verificaImpar :: Int -> Bool
verificaImpar n = (n `mod` 2) == 1

-- filtrar :: (a -> Bool) -> [a] -> [a]
filtrar :: (t -> a) -> [t] -> [a]
filtrar _ [] = []
filtrar a (x:xs) = (a x):filtrar a xs

main :: IO ()
main = do
    print(filtrar verificaImpar [115,52,51,92,84,5,8949])