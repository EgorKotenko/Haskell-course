-- 1 способ

evenElements1 list = length (filter (\x -> rem x 2 == 0) list)
	
-- 2 способ

evenElements2 list = length (filter ( == 0) (map (`rem` 2) list)) 

-- 3 способ
	
evenElements3 list = (length list) - foldr (+) 0 (map (`rem` 2) list)

	