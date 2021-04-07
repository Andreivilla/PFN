data Tree a = Leaf
            | Branch (Tree a) a (Tree a)

atravessar :: (a -> b) -> Tree a -> Tree b
atravessar f Leaf = Leaf
atravessar f (Branch esq a dir) = Branch (atravessar f esq) (f a) (atravessar f dir)