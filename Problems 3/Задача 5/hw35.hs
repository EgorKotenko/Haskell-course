data Tree a = Leaf a | Branch (Tree a) a (Tree a)

minPath (Leaf _) = 0
minPath (Branch xl _ xr) = 1 + min (minPath xl) (minPath xr)