

import ProblemSet10

--Modify the result of problem 10 in such a way that if an element 
--has no duplicates it is simply copied into the result list. 
--Only elements with duplicates are transferred as (N E) lists.
data EncodeNode a = Single a | Multiple a Int 
    deriving (Read, Show, Eq, Ord)
    
encodeNode :: (t, Int) -> EncodeNode t    
encodeNode (ch, count)
    | count == 1        = Single ch 
    | otherwise         = Multiple ch count 

decodeNode :: EncodeNode t -> [t]
decodeNode (Single a) = expand a 1
decodeNode (Multiple a b) = expand a b

expand :: (Num a) => t -> a -> [t]
expand c n 
    | n == 1        = [c]
    | otherwise     = c:(expand c (n-1))
    
encodeModified :: (Eq t) => [t] -> [EncodeNode t]
encodeModified [] = []
encodeModified xs = map encodeNode (encode xs)

--Decode a run-length encoded list
decodeModified :: [EncodeNode b] -> [b]
decodeModified [] = []
decodeModified xs = concatMap decodeNode xs 


--Run-length encoding of a list (direct solution).
encodeDirect :: (Eq a) => [a] -> [EncodeNode a]
encodeDirect [] = []
encodeDirect (x:xs) 
    = (code (x : takeWhile (==x) xs)) : encodeDirect (dropWhile (==x) xs)
        where 
            code [x] = Single x 
            code (x:xs) = Multiple (head xs) ((length xs)+1) 

--Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli xs = repli xs 2 

--Replicate the elements of a list a given number of times
expand_mod n xs = expand xs n
repli :: (Num a) => [b] -> a -> [b]
repli xs n = concatMap (expand_mod n) xs 

--Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [xs!!i | i <- [0..(length xs)-1], mod (i+1) n /= 0]

--Split a list into two parts; the length of the first part is given
split :: [a] -> Int -> ([a], [a])
split xs n = ([xs!!i | i <- [0..n-1]], [xs!!i | i <- [n..(length xs)-1]])

--Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice xs a b = fst (split part_xs (b-a+1))
    where part_xs = snd (split xs (a-1))

--Rotate a list N places to the left
rotate :: [a] -> Int -> [a]
rotate xs n 
    | n >= 0        = (snd split_xs) ++ (fst split_xs)
    | otherwise     = rotate xs (length xs + n)
        where split_xs = split xs n 
                      

--Remove the K'th element from a list
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs!!n, (fst split_xs) ++ (tail (snd split_xs)))
    where split_xs = split xs n 


