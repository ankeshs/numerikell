{-|
	Licence Information

	This file is part of Numerikell 1.0.0.0 Haskell Numerical
	Software project. Please do not share, copy, edit or distribute
	without owner's permission.  

	@Contributors : Please striclty follow Haskell community convention.
	Comment your code, use proper nomenclature for functions, variables
	and modules.

	File Specification :
	Contributor : Dipendra K. Misra (dipendrakumarmisra@gmail.com),
-}

module Number (
  gcdEuclid,
  prime,
  fibonacci,
  fibonacciSeries,
  fact,
  combine,
  legendre
  )
  where


-- finds gcd of two numbers using Euclid method
gcdEuclid :: Int -> Int -> Int
gcdEuclid a b = if (b==0)
          then a
          else gcdEuclid b (mod a b)

-- check if a number is prime
prime :: Int -> Bool
prime n = if (r==0) then True else False
        where r = length (filter (\u -> u==0) (map (\t -> mod n t ) [2..(n-1)]))

-- generate nth Fibnonacci Number
fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- generate Fibonacci Series
fibonacciSeries :: [Int]
fibonacciSeries = map (\t -> fibonacci t) [0..]

-- factorial computation
fact :: Int -> Int
fact 0 = 1
fact n = n*(fact (n-1))

-- combinatorics
combine :: Int -> Int -> Int
combine n k = quot (fact n) ((fact k) * (fact (n-k)))

-- Legendre Symbol
legendre :: Int -> Int -> Int
legendre a p = mod (a^(quot (p-1) 2)) p

