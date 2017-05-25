data (Ord a) => BinarySearchTree a = Nil | Node a (BinarySearchTree a) (BinarySearchTree a) deriving (Eq, Show)

-- Высота
height :: (Ord a) => BinarySearchTree a -> Int
height Nil  = 1
height (Node _ Nil Nil) = 1
height (Node _ xl xr) = 1 + max (height xl) (height xr) 

-- Количество элементов
elements :: (Ord a) => BinarySearchTree a -> Int
elements Nil = 0
elements (Node _ xl xr) = 1 + (elements xl) + (elements xr)

-- Проверка элемента на присутствие в дереве
exist :: (Ord a) => a -> BinarySearchTree a -> Bool
exist input Nil = False
exist input (Node x xl xr)
    | input < x = exist input xl
    | input > x = exist input xr
    | otherwise = True

-- Создание дерева
createTree :: (Ord a) => a -> BinarySearchTree a
createTree input = Node input Nil Nil

-- Добавление элемента  
insert :: (Ord a) => a -> BinarySearchTree a -> BinarySearchTree a
insert input Nil = createTree input
insert input (Node x xl xr)
    | input < x = Node x (insert input xl) xr
    | input > x = Node x xl (insert input xr)
    | otherwise = Node x xl (insert input xr)
 
-- Удаление элемента 
remove :: (Ord a) => a -> BinarySearchTree a -> BinarySearchTree a
remove _ Nil = error "Empty tree"
remove input (Node x xl xr)
    | input < x     = Node x (remove input xl) xr
    | input > x     = Node x xl (remove input xr)
    | otherwise = keepBalanced xl xr
    where  keepBalanced :: (Ord a) => BinarySearchTree a -> BinarySearchTree a -> BinarySearchTree a
           keepBalanced Nil xr = xr
           keepBalanced xl Nil = xl
           keepBalanced xl xr = Node (findRightMost xl) (remove (findRightMost xl) xl) xr
               where findRightMost :: (Ord a) => BinarySearchTree a -> a
                     findRightMost (Node rm _ Nil) = rm
                     findRightMost (Node _  _ r)   = findRightMost r
 
