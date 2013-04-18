-- |This modules provides functions used for Numerical logic
module Number (
  -- * functions
  gcdEuclid,
  prime,
  fibonacci,
  fibonacciSeries,
  fact,
  combine,
  legendre,
  )
  where


-- |finds gcd of two numbers using Euclid method
gcdEuclid :: Int -> Int -> Int
gcdEuclid a b = if (b==0)
          then a
          else gcdEuclid b (mod a b)

-- |check if a number is prime
prime :: Int -> Bool
prime n = if (r==0) then True else False
        where r = length (filter (\u -> u==0) (map (\t -> mod n t ) [2..(n-1)]))

-- |generate nth Fibnonacci Number
fibonacci :: Int -> Int
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- |generate Fibonacci Series
fibonacciSeries :: [Int]
fibonacciSeries = map (\t -> fibonacci t) [0..]

-- |factorial computation
fact :: Int -> Int
fact 0 = 1
fact n = n*(fact (n-1))

-- |combinatorics
combine :: Int -> Int -> Int
combine n k = quot (fact n) ((fact k) * (fact (n-k)))

-- |Legendre Symbol
legendre :: Int -> Int -> Int
legendre a p = mod (a^(quot (p-1) 2)) p

