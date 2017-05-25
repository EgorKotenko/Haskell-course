reverseList :: [Int] -> [Int]
reverseList list = fromFirstToSecond list [] where 
	fromFirstToSecond :: [Int] -> [Int] -> [Int]
	fromFirstToSecond [] temp = temp
	fromFirstToSecond (x:xs) temp = fromFirstToSecond xs (x:temp)