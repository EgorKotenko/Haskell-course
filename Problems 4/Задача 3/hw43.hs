-- 1 ������

evenElements1 list = length (filter (\x -> rem x 2 == 0) list)
	
-- 2 ������

evenElements2 list = length (filter ( == 0) (map (`rem` 2) list)) 

-- 3 ������
	
evenElements3 list = (length list) - foldr (+) 0 (map (`rem` 2) list)

	