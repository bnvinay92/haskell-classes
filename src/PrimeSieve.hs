module PrimeSieve where

-- A prime number x is a natural number not divisible by any prime numbers less than equal to square root of x.
-- 2 is the first prime number.
isPrime :: Integer -> [Integer] -> Bool
isPrime x somePrimes = all (\p -> x `mod` p /= 0) (filter lessThanSqrtOfX somePrimes)
  where
    lessThanSqrtOfX z = z * z <= x

nPrimes' n primes i
  | length primes == n = primes
  | otherwise =
    let primes' =
          if isPrime i primes
            then i : primes
            else primes
    in nPrimes' n primes' (i + 1)

nPrimes n = nPrimes' n [2] 3

primeFold :: [Integer]-> Integer -> [Integer]
primeFold ps c =
  if isPrime c ps
    then c : ps
    else ps
