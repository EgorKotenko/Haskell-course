import List

type HashTable = [ [Char] ] 

hashFunc :: [Char] -> Int
hashFunc str = rem (17 * (f str)) 13 where 
	f [] = 0
	f (x:xs) = fromEnum x + f xs

-- add
add' table str = insert str (table !! (hashFunc str))

-- delete
delete' table str = delete str (table !! (hashFunc str))

-- find
find' table str = table !! (hashFunc str)




