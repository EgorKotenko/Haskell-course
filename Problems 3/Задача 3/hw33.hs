maxPair :: [Int] -> Int
maxPair list =  firstTime (zipWith (+) (0:list) list) (maximum (zipWith (+) (0:list) list))
   
firstTime :: [Int] -> Int -> Int
firstTime list n = stepNumber list n 0 where
	stepNumber :: [Int] -> Int -> Int -> Int 
	stepNumber [] _ k = error " "
	stepNumber (x:xs) n k = if x == n then k else stepNumber xs n k+1
	
	