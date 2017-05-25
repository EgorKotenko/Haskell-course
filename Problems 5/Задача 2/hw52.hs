--check f (x:xs)
--	| f x = check f xs 
--	| otherwise = False
--check _ [] = True

check f list = length (filter f list) == length list