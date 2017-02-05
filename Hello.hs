module Hello where

import Data.Char
main = putStrLn "Hello, World!"

sumSquare x y = x^2 + y^2

lenVec3 x y z = sqrt (x^2 + y^2 + z^2)

sign x = if x == 0 then 0 else if x > 0 then 1 else -1

infixl 7 **+
(**+) a b = a*a + b*b

infixl 6 *+*
(*+*) a b = a ^ 2 + b ^ 2

infixl 6 |-|
(|-|) a b = if a - b < 0 then -(a - b) else a - b

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int a b = if isDigit a && isDigit b then digitToInt a * 10 + digitToInt b else 100

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ ((^ 2) $ fst p2 - fst p1) + ((^ 2) $ snd p2 - snd p1)
