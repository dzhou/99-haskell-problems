

module Arithmetic where
import Lists

--(**) Determine whether a given integer number is prime.
isPrime :: (Integral a) => a -> Bool
isPrime n 
    | n < 2         = False
    | n == 2        = True
    | n == 3        = True 
    | otherwise     = (all (\x -> mod n x /= 0) [2..(n-1)])

--Determine the greatest common divisor of two positive integer numbers. 
--Use Euclid's algorithm 
myGCD :: Integral a => a -> a -> a 
myGCD a b 
    | b == 0        = a
    | otherwise     = myGCD b (mod a b)

--(*) Determine whether two positive integer numbers are coprime. 
--Two numbers are coprime if their greatest common divisor equals 1.
coprime :: (Integral a) => a -> a -> Bool
coprime a b = (myGCD a b) == 1 

--Calculate Euler's totient function phi(m).
totient :: (Integral a) => a -> Int
totient m 
    | m < 1         = 0
    | m == 1        = 1 
    | otherwise     = length [i | i <- [1..m-1], coprime m i]

    
--Determine the prime factors of a given positive integer. 
--Construct a flat list containing the prime factors in ascending order 
primeFactors :: (Integral a) => a -> [a]
primeFactors  n 
    | n <= 1         = []
    | n == 2        = [2]
    | otherwise     = next_p : (primeFactors  (n `div` next_p))
        where next_p = head [p | p <- [2..n], mod n p == 0, isPrime p]

--Determine the prime factors of a given positive integer
prime_factors_mult n = (encode . primeFactors) n 

--Calculate Euler's totient function phi(m) (improved).
--totient_improved m = foldr fn 0 (prime_factors_mult m) 
--    where fn (a,b) (c,d) = a+c
totient_improved m = totient' 0 (prime_factors_mult m)
    where 
        totient' n [] = n
        totient' n ((p,c):xs) = totient' (n+(p-1)*p^(c-1)) xs 

--Given a range of integers by its lower and upper limit, construct a 
--list of all prime numbers in that range
primesR a b = [p | p<-[a..b], isPrime p]

--Goldbach's conjecture
goldbach n = (p, n-p)
    where p = head [p1 | p1<-(primesR 1 n), isPrime (n-p1)]

--Given a range of integers by its lower and upper limit, print 
--a list of all even numbers and their Goldbach composition
goldbachList a b 
    | a >= b        = []
    | a <= 2        = map goldbach [4,6..b]
    | mod a 2 == 0  = map goldbach [a,a+2..b]
    | otherwise     = map goldbach [a+1,a+3..b]


