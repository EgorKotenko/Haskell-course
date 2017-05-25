import Random

data Tree a = Nill | Node a (Tree a) (Tree a) deriving (Show,Eq)

-- f list = take (length list) (randoms (mkStdGen 42) :: [Int])

leaf x = Node x Nill Nill

treeToList Nill = []
treeToList (Node x xl xr) = x : treeToList xl ++ treeToList xr

listToTree list = foldl (\tre x -> insert x tre) Nill list

insert a Nill = leaf a
insert a (Node x xl xr) | a < x = Node x (insert a xl) xr
                        | otherwise = Node x xl (insert a xr)

res a = listToTree (take (length (treeToList a)) (randoms (mkStdGen 42) :: [Int]))
