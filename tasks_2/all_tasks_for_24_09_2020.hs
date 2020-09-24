import Data.List
-- #1 get all primes with Erathostene's sieve
infinitePrimes::[Int]
infinitePrimes = erathSieve [2 .. ]
erathSieve (p:xs) = p:erathSieve [x | x <- xs, x `mod` p /= 0]
-- take 21 infinitePrimes

-- #2 unfold in binary presentation
unfoldX::Int -> [Int]
unfoldX = reverse . unfoldr (\x -> if x==0 then Nothing else Just(rem x 2, div x 2))
