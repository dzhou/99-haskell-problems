
module Lists3 where
import Lists2

import System.Random hiding (split)
import Control.Monad.State
import Control.Monad (replicateM)
import Data.List hiding (permutations)

--Insert an element at a given position into a list.
insertAt x xs n = (fst split_xs) ++ [x] ++ (snd split_xs)
    where split_xs = split xs n 

--Create a list containing all integers within a given range.
range a b = [a..b]

--Extract a given number of randomly selected elements from a list.
--version 1 - use removeAt, results is ordered 
rnd_select' :: (RandomGen t) => [a] -> Int -> t -> ([a], t)
rnd_select' ls count gen 
    | count == (length ls)  = (ls, gen)
    | otherwise             = rnd_select' (snd (removeAt k ls)) count gen'
                                where (k, gen') = randomR (0, (length ls)-1) gen

--Version 2 - unorder results, can be used for permutation later 
rnd_select'' :: (RandomGen t) => [a] -> Int -> t -> ([a], t)
rnd_select'' ls count gen 
    | count == 0            = ([], gen)
    | otherwise             = (n : ks, gen'')
                                where (n, ls', gen') = removeRandom ls gen
                                      (ks, gen'') = rnd_select'' ls' (count-1) gen'

removeRandom :: (RandomGen t) => [a] -> t -> (a, [a], t)
removeRandom ls gen = (n, ls', gen')
    where (n, ls') = removeAt k ls 
          (k, gen') = randomR (0, (length ls)-1) gen


rnd_select :: [a] -> Int -> IO [a]
rnd_select ls count = getStdRandom $ rnd_select' ls count

--Lotto: Draw N different random numbers from the set 1..M.
diff_select :: (Num t, Enum t) => Int -> t -> IO [t]
diff_select n m = rnd_select [1..m] n 

--Generate a random permutation of the elements of a list.
rnd_permutation ls = getStdRandom $ rnd_select'' ls (length ls)

--Generate the combinations of K distinct objects chosen from the 
--N elements of a list
combinations :: Int -> [a] -> [[a]]
combinations n xs = case (n, xs) of     
        (0, _)          -> [[]]
        (_, [])         -> []
        (n, x:xs)       -> (map (x:) (combinations (n-1) xs)) ++ (combinations n xs)

--permutation with replacement 
permutations :: (Num a) => a -> [Char] -> [[Char]]
permutations n items 
    | n == 0            = []
    | n == 1            = permutations' [""] items 
    | otherwise         = permutations' (permutations (n-1) items) items 
        where permutations' [] items = []
              permutations' (p:perms) items = (map (:p) items) ++ 
                permutations' perms items 


--Group the elements of a set into disjoint subsets.


--Sorting a list of lists according to length of sublists
--a) We suppose that a list contains elements that are lists themselves. 
--The objective is to sort the elements of this list according to their 
--length. E.g. short lists first, longer lists later, or vice versa. 
lsort :: [[a]] -> [[a]]
lsort  [] = []
lsort (x:xs) = lsort (filter (cmplen (<=) x) xs) ++ [x] ++ lsort (filter (cmplen (>) x) xs)
    where cmplen op a b = op (length b) (length a)

--b) Again, we suppose that a list contains elements that are lists 
--themselves. But this time the objective is to sort the elements of this 
--list according to their length frequency 
lfsort xs = flatten $ lsort $ groupBy (\x y -> length x == length y) (lsort xs)
    where flatten [] = []
          flatten (x:xs) = x ++ flatten xs 

