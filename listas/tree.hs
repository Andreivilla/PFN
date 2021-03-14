{-- 
data Tree a = Empty | Branch a (Tree a) (Tree a)
arv = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                                (Branch 'e' Empty Empty))
                    (Branch 'c' Empty
                                (Branch 'f' (Branch 'g' Empty Empty)
                                            Empty))
--}
{--
data Tree = Leaf Int | Node Int Tree Tree
ex1 = Leaf 1
ex2 = Node 2 (Leaf 1) (Node 3 (Leaf 0) (Leaf 5))
--}
--arvores para testes
data Tree a = Leaf a | Branch a (Tree a) (Tree a)
arv = Branch 1 (Leaf 9) (Branch 3 (Leaf 0) (Leaf 5))
arv1 = Branch 0 (Leaf 1) (Branch 2 (Leaf 3) (Branch 4 (Branch 5 (Leaf 6) (Leaf 7)) (Leaf 8)))
arv2 = Branch 1 (Leaf 4) (Leaf 8)

--ex19--
ler :: Tree a -> [a]
ler (Leaf a) = [a]
ler (Branch a esq dir) = ler esq ++ [a] ++ ler dir

maiorList :: Ord a => [a] -> a 
maiorList [a] = a
maiorList (x:xs) = if x>maiorList xs then x else maiorList xs

maior :: Ord a => Tree a -> a
maior a = maiorList(ler(a))

--ex20--
lerR :: Tree a -> [a]
lerR (Leaf a) = [a]
lerR (Branch a esq dir) = [a] ++ lerR dir

lerL :: Tree a -> [a]
lerL (Leaf a) = [a]
lerL (Branch a esq dir) = lerL esq ++ [a]


altura :: Tree a -> Int
altura (Leaf a) = 1
altura (Branch a esq dir) = if altura esq > altura dir then (altura esq + 1) else (altura dir + 1)


main :: IO ()
main = do
    --print(ler arv1)
    --print(maiorList[1,5,8,9,7,6,18])
    --print(maior(arv))
    --print(altura(arv1))
    print(altura(arv1))


