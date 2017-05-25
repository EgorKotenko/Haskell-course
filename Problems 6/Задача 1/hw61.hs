data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

treeToString :: Tree [Char] -> [Char]
treeToString Empty = "e"
treeToString (Node x xl xr) = 'n' : x ++ treeToString xl ++ treeToString xr

stringToTree :: [Char] -> Tree Char
stringToTree [] = Empty
stringToTree (x:xs)
	| x == 'e' = Empty
	| otherwise = Node (head xs) (stringToTree (fst (f (tail xs) [] 0))) (stringToTree (snd (f (tail xs) [] 0)))

f :: [Char] -> [Char] -> Int -> ([Char], [Char])
f [] cur _ = (cur, [])
f (x:xs) cur k
	| x == 'n' = f (tail xs) (cur ++ [x] ++ [head xs]) (k + 1)
    | x == 'e' && (k == 0) = (cur ++ [x], xs)
    | x == 'e' = f xs (cur ++ [x]) (k - 1)
    