
module BinaryTrees where


data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

--Construct completely balanced binary trees
cbalTree :: (Integral t) => t -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = [Branch 'x' left right | i <- [q..q+r],
                                      left <- cbalTree i,
                                      right <- cbalTree $ n - i - 1]
             where (q, r) = quotRem (n-1) 2


--Symmetric binary trees
hash :: Tree t -> [Char]
hash Empty = "0"
hash (Branch a b c) = "1" ++ hash b ++ hash c

hash' :: Tree t -> [Char]
hash' Empty = "0"
hash' (Branch a b c) = "1" ++ hash c ++ hash b

symmetric :: Tree t -> Bool 
symmetric Empty = True
symmetric (Branch a b c) = hash b == hash' c 


--binary search trees (dictionaries)
construct :: (Ord a) => [a] -> Tree a
construct [] = Empty
construct [a] = Branch a Empty Empty
construct (n:ns) = Branch n (construct left) (construct right)
    where left = filter (<=n) ns
          right = filter (>n) ns

--Generate-and-test paradigm
--Apply the generate-and-test paradigm to construct all symmetric, completely
--balanced binary trees with a given number of nodes
symCbalTrees n = filter symmetric $ cbalTree n

--In a height-balanced binary tree, the following property holds for every node:
--The height of its left subtree and the height of its right subtree are almost
--equal, which means their difference is not greater than one
hbalTree :: (Num t) => t -> [Tree Char]
hbalTree 0 = [Empty]
hbalTree 1 = [Branch 'x' Empty Empty]
hbalTree h = [Branch 'x' left right | 
                    (hl, hr) <- [(h-1, h-2), (h-2, h-1), (h-1, h-1)],
                    left <- hbalTree hl,
                    right <- hbalTree hr]


--Construct height-balanced binary trees with a given number of nodes
countNodes :: (Num a) => Tree t -> a
countNodes Empty = 0
countNodes (Branch a b c) = 1 + countNodes b + countNodes c

minNodes :: (Num a) => a -> a   
minNodes 0 = 0
minNodes 1 = 1
minNodes h = 1 + minNodes (h-1) + minNodes (h-2)

maxHeight :: (Num a, Ord a, Enum a) => a -> a 
maxHeight n = last $ takeWhile (\x -> minNodes x <= n) [1..]     

hbalTreeNodes :: (Num a, Ord a, Enum a) => a -> [Tree Char]
hbalTreeNodes n = concatMap filtertree [0..(maxHeight n)]
            --where filtertree = filter ((n==) . countNodes) . hbalTree
            where filtertree = filter (\x -> countNodes x == n) . hbalTree 


