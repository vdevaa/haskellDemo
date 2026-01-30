--- simplest problem

import Data.Char
import Data.Text.Lazy.Read (double)

-- Pattern Matching
charCat 'a' = "first alphabet"
charCat 'z' = "last alphabet"
charCat _ = "middle alphabet" -- wildcard pattern

-- Siminalr to switch case (called case of)
charCat2 c = case c of 
    'a' -> "first alphabet"
    'z' -> "last alphabet"
    _ -> "middle alphabet"


-- Guards
charCat3 c
    | c == 'a' = "first alphabet"
    | c == 'z' = "last alphabet"
    | otherwise = "middle alphabet"

-- VSCODE will not like this format and make it blue, this is okay
charCat4 c = 
    if c == 'a'
        then "first alphabet"
        else if c == 'z'
            then "last alphabet"
            else "middle alphabet"

-- Global Scope (Variables) (Literals)
x = 20
y = True
z = 'd'
name = "Vijay"

-- functions
inc = (\x -> x + 1) -- reason why x does not affect the x in line 29 is because of scope
inc5 = (\x -> x + 5)
inc10 = (\x -> x + 10)

-- high order function
incMaker = (\incVal -> (\x -> x + incVal))


-- F(x,y) = x + 2y
-- y = 2
-- P(x) = x + 4

-- Currying (Haskell Currying)
add x y = x + y

add' = (\x -> (\y -> x + y))

add'' = (\x y -> x + y)

-- in haskell we have list and tuples (they need to be homogenous)

-- F(x) = x + 27
-- g(x) = F(x)
-- g = f
sum1 = add
num = 10
num1 = num


-- Function Composition
-- f(x) = 2 * (x + 1)
-- g(x) = 2x + 1
-- k(x) = f(g(f(x)))
-- k = f . g . x
f x = 2 * (x + 1)
g x = (2 * x) + 1
k = f(g(f(x)))
k' = f . g . f


-- Point Free Style
toUpperStr = map toUpper
doubleList xs = map (*2) xs
doubleList' = map (*2)

-- How to interate over list
myLen [] = 0
myLen (_:xs) = 1 + myLen xs

posNeg [] = ([],[])
posNeg (x:xs)
    | x > 0 = (x:first, second)
    | otherwise = (first, x:second)
    where (first, second) = posNeg xs










