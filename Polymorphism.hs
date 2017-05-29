module Polymorphism where
import Data.Function

getSecondFrom x y z = y

myf :: a -> a -> b -> a -> a
myf x y z a = x

multSecond = g `on` h

g = (*)
h x = snd x


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)


-- Функция одной переменной doItYourself выбирает наибольшее из переданного ей аргумента и числа 42, затем возводит результат выбора в куб и, наконец, вычисляет логарифм по основанию 2 от полученного числа. Эта функция реализована в виде:

doItYourself = f . g . h

f = undefined
g = undefined
h = undefined