notExist a [] = True
notExist a (x:xs)
	| x == a = False
	| otherwise = notExist a xs
	
notEqual [] = True
notEqual (x:xs)
	| notExist x xs == True = notEqual xs
	| otherwise = False
	
	

	