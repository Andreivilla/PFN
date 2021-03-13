filtrar :: (t -> a) -> [t] -> [a]
filtrar _ [] = []
filtrar a (x:xs) = (a x):filtrar a xs