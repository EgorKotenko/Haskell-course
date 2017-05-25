data Tree a = Leaf a | Branch (Tree a) a (Tree a)

foldl' f z (Leaf x) = f z x
foldl' f z (Branch xl x xr) =  foldl' f (foldl' f (f z x) xl) xr 
