data Tree a = Leaf a | Branch a (Tree a) (Tree a)

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

arv = Branch 1 (Leaf 2) (Branch 3 (Leaf 0) (Leaf 5))
arv2 = Branch 1 (Leaf 4) (Leaf 8)

-- arv = Leaf 'a' 




todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = if x == True then todos xs else False

main :: IO ()
main = do
    print(todos [True, True, True, True])


