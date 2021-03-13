ultimoElemento :: [a] -> a
ultimoElemento [x] = x
ultimoElemento (x:xs) = ultimoElemento xs

-- lembrando q a função last faz isso tambem, sendo assim a funcão tambem poderia ser feita da seguinte forma
-- ultimoElemento :: [a] -> a
-- ultimoElemento a = last a