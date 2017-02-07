module Recursion where

fact :: Integer -> Integer
fact 0 = 1
fact n | n > 0 = n * fact (n - 1)
       | otherwise = error "argument must be not negative"

fact' :: Integer -> Integer
fact' n | n >= 0 = helper' 1 n
        | otherwise = error "arg must not be negative"

helper' acc 0 = acc
helper' acc n = helper' (acc * n) (n - 1) 

doubleFact :: Integer -> Integer
doubleFact n | n == 0 = 1
             | n == 1 = 1
             | n < 0 = error "argument must be not negative"
             | n > 0 = n * doubleFact (n-2)

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)


fibonacci' :: Integer -> Integer
fibonacci' n = 
        let helper n a b 
                | n == 0 = a
                | n > 0  = helper (n-1) b (a+b)
                | n < 0  = helper (n+1) b (a-b)
        in helper n 0 1

seqA :: Integer -> Integer
seqA n = 
      let helper n a b c
            | n < 3 = n+1
            | n == 3 = (c+b-2*a)
            | n > 0 = helper (n-1) b c (c+b-2*a)
      in helper n 1 2 3

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = 
      let helper x sum count
            | absX > 9 = helper (quot absX 10) (sum + digit) (count + 1)
            | otherwise = (sum + digit, count + 1)
            where digit = rem absX 10
                  absX = abs x
      in helper x 0 0