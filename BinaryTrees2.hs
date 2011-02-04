
module BinaryTrees2 where
import BinaryTrees

--Count the leaves of a binary tree
countLeaves :: (Num t1, Eq t) => Tree t -> t1
countLeaves Empty = 0
countLeaves (Branch a b c) 
    | b == Empty && c == Empty      = 1
    | otherwise                     = countLeaves b + countLeaves c


--Collect the leaves of a binary tree in a list
leaves :: (Eq a) => Tree a -> [a]
leaves Empty = []
leaves (Branch a b c) 
    | b == Empty && c == Empty      = [a] 
    | otherwise                     = leaves b ++ leaves c 

--Collect the internal nodes of a binary tree in a list
internals :: (Eq a) => Tree a -> [a]
internals Empty = []
internals (Branch a b c) 
    | b == Empty && c == Empty      = []
    | otherwise                     = [a] ++ internals b ++ internals c 

--Collect the nodes at a given level in a list
atLevel :: (Num t) => Tree a -> t -> [a]
atLevel Empty n = []
atLevel (Branch a b c) 1 = [a]
atLevel (Branch a b c) n = atLevel b (n-1) ++ atLevel c (n-1)




