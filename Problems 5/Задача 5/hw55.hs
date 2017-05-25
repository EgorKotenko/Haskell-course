f :: [Int] -> [(Int, Int)]
f list = zip [0..((length list) - 1)] list

g :: [Int] -> [Int]
g list = f list >>= (\x -> h x list)

h :: (Int, Int) -> [Int] -> [Int]
h x list
	| ((not (fst x == 0)) && (not (fst x == (length list) - 1)) && (snd x > snd ( (f list) !! ((fst x) - 1))) && (snd x > snd ( (f list) !! ((fst x) + 1)))) == True = [snd x]
	| otherwise = []
	
func :: [Int] -> Int	
func list = head (g list)