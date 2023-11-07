module BinaryTree (Tree (..), insert, search, delete, findMin, findMax, findMaxVal, findMinVal) where

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Read, Eq)

instance (Show a) => Show (Tree a) where
  show Empty = ""
  show tree = show' tree 0 (widestElement tree + 1)

insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node val left right)
  | x == val = Node val left right
  | x < val = Node val (insert x left) right
  | x > val = Node val left $ insert x right
  | otherwise = Empty

search :: (Ord a) => a -> Tree a -> Tree a
search _x Empty = Empty
search x (Node val left right)
  | x == val = Node val left right
  | x < val = search x left
  | x > val = search x right
  | otherwise = Empty

delete :: (Ord a) => a -> Tree a -> Tree a
delete _x Empty = Empty
delete x (Node val left right)
  | x == val = deleteNode (Node val left right)
  | x < val = Node val (delete x left) right
  | x > val = Node val left (delete x right)
  | otherwise = Empty

deleteNode :: (Ord a) => Tree a -> Tree a
deleteNode (Node _val Empty Empty) = Empty
deleteNode (Node _val Empty right) = right
deleteNode (Node _val left Empty) = left
deleteNode (Node _val left right) = Node (findMinVal right) left (delete (findMinVal right) right)
deleteNode Empty = Empty

findMin :: (Ord a) => Tree a -> Tree a
findMin Empty = Empty
findMin (Node val Empty right) = Node val Empty right
findMin (Node _val left _right) = findMin left

findMinVal :: (Ord a) => Tree a -> a
findMinVal Empty = error "Empty tree"
findMinVal (Node val Empty _right) = val
findMinVal (Node _val left _right) = findMinVal left

findMax :: (Ord a) => Tree a -> Tree a
findMax Empty = Empty
findMax (Node val left Empty) = Node val left Empty
findMax (Node _val _left right) = findMax right

findMaxVal :: (Ord a) => Tree a -> a
findMaxVal Empty = error "Empty tree"
findMaxVal (Node val _left Empty) = val
findMaxVal (Node _val _left right) = findMaxVal right

show' :: (Show a) => Tree a -> Int -> Int -> String
show' Empty _ _ = " "
show' (Node a left right) depth width =
  leftside ++ "\n" ++ center ++ rightside
  where
    center = replicate depth ' ' ++ show a
    leftside = show' left (depth + width) width
    rightside = show' right (depth + width) width

widestElement :: (Show a) => Tree a -> Int
widestElement Empty = 0
widestElement (Node center left right) = maximum [l, r, c]
  where
    l = widestElement left
    r = widestElement right
    c = length $ show center
