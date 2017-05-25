data Tree a = Leaf a | Branch (Tree a) a (Tree a)

height (Leaf _) = 1
height (Branch xl _ xr) = 1 + max (height xl) (height xr)