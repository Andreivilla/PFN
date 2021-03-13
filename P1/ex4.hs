maiorElemento :: Ord a => [a] -> a
maiorElemento [x] = x
maiorElemento (x:xs) = if x>maiorElemento xs then x else maiorElemento xs