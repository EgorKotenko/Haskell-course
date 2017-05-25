import List

type Polinomial = [(Int, Int)] 

-- Умножение
mult' :: Polinomial -> Polinomial -> Polinomial
mult' _ [] = []
mult' a (x:xs) = sum' (h a x) (mult' a xs)

h :: Polinomial -> (Int, Int) -> Polinomial
h [] _ = []
h (x:xs) n = ((fst x) * (fst n), (snd x) + (snd n)) : (h xs n)

-- Сложение
sum' :: Polinomial -> Polinomial -> Polinomial
sum' a b = g a (f a b) 

f :: Polinomial -> Polinomial -> Polinomial
f _ [] = [] 
f a (x:xs) = ((fst x) + findFactor (snd x) a, snd x) : f a xs  

g :: Polinomial -> Polinomial -> Polinomial
g [] c = c 
g (x:xs) c 
	| notExist (snd x) c == True = g xs (insertBy add (fst x, snd x) c)
	| otherwise = g xs c 
	where add a b
			| snd a > snd b = LT
			| otherwise = GT
		
notExist :: Int -> Polinomial -> Bool
notExist power [] = True
notExist power (x:xs)
	| snd x == power = False
	| otherwise = notExist power xs

findFactor :: Int -> Polinomial -> Int
findFactor power [] = 0
findFactor power (x:xs)
	| snd x == power = fst x
	| otherwise = findFactor power xs