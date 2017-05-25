isCorrect :: [Char] -> Bool
isCorrect string = correct string 0 where
	correct :: [Char] -> Int -> Bool 
	correct [] k = if (k == 0) then True else False
	correct (x:xs) k = if (k >= 0) then ( if (x == '(') then correct xs (k+1) else if (x == ')') then correct xs (k-1) else correct xs k) else False