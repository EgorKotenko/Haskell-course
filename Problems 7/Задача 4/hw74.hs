-- map

map' :: (a -> b) -> [a] -> [b]
map' f list = map_cps g list where
	g = \x y -> (:) (f x) y

map_cps f [] = []
map_cps f (x:xs) = f x (map_cps f xs)

-- filter

filter' :: (a -> Bool) -> [a] -> [a]
filter' f list = filter_cps h list where
	h = \x y -> if (f x) then (:) x y else y

filter_cps f [] = []
filter_cps f (x:xs) = f x (filter_cps f xs)