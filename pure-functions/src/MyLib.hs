module MyLib (add, sub, fac) where

add :: Num a => a -> a -> a
add x y = x + y

sub :: Num a => a -> a -> a
sub x y = x - y

fac :: (Eq t, Num t) => t -> t
fac x = if x == 0 then 1 else x * fac (x - 1)