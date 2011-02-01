

module Lists where

--Find the last element of a list.
myLast :: [a] -> a
myLast (x:xs)
    | length xs == 0    = x
    | otherwise         = myLast xs

--Find the last but one element of a list
myButLast :: [a] -> a
myButLast (x:xs)
    | length xs == 1    = x
    | otherwise         = myButLast xs

--(*) Find the K'th element of a list. The first element in the list is number 1.
elementAt :: (Num b) => [a] -> b -> a
elementAt (x:xs) n
    | n == 1        = x
    | otherwise     = elementAt xs (n-1)
    
--Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs 

--Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (reverse xs) ++ [x]

--Find out whether a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool 
isPalindrome xs = xs == (myReverse xs)

--Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x 

--Eliminate consecutive duplicates of list elements
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) 
    | x == y        = compress (y:xs)
    | otherwise     = x : (compress (y:xs))

--Pack consecutive duplicates of list elements into sublists. 
--If a list contains repeated elements they should be placed in separate sublists
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

--(*) Run-length encoding of a list. Use the result of problem P09 to implement 
--the so-called run-length encoding data compression method. Consecutive 
--duplicates of elements are encoded as lists (N E) where N is the number of 
--duplicates of the element E.
--encode_node :: [a] -> (a, Int)
--encode_node xs = (head xs, length xs)
encode :: (Eq a) => [a] -> [(a, Int)]
encode [] = []
encode xs = map (\x -> (head x, length x)) (pack xs)
    

