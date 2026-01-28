module MyLib (add, sub, fac) where
import Data.ByteString (find)

add :: Num a => a -> a -> a
add x y = x + y

sub :: Num a => a -> a -> a
sub x y = x - y

fac :: (Eq t, Num t) => t -> t
fac x = if x == 0 then 1 else x * fac (x - 1)


testAdd1 = if add 1 2 == 3
    then "test passed"
    else "test failed"

runTests = do
    putStrLn testAdd1

findElem x [] = False
findElem x (y:ys) = if x == y
    then True
    else findElem x ys