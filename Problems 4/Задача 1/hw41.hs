import List

data Tree a = Leaf a | Branch (Tree a) a (Tree a)

treeSearch f (Leaf x) = search f (Leaf x) []
treeSearch f (Branch xl x xr) = search f (Branch xl x xr) []
 
search f (Leaf x) list
	| f x = (x:list)
	| otherwise = list
search f (Branch xl x xr) list 
	| f x = (x:(union (search f xl list) (search f xr list)))
	| otherwise = union (search f xl list) (search f xr list)