--- Implement drop

-- How to debug

import Debug.Trace (trace)


dropLs n xs| trace (" n = " ++ show n ++ " xs = " ++ show xs) False = undefined
dropLs n [] = []
dropLs n ls@(x:xs)
    | n > 0 = dropLs (n - 1) xs
    | otherwise = ls

--- Implement reseverse

rev [] = []
rev(x:xs) = (rev xs) ++ [x] -- This is not efficient because of the use of ++

-- something better/more efficient

revHelper [] acc = acc
revHelper (x:xs) acc = revHelper xs (x:acc)


revs xs = revHelper xs []


--- Odds and Evens

-- if we call oddsEvens [1,2,3,4,5] we want to get ([1,3,5], [2,4])

oddEvens xs = (odds, evens)
    where
        odds = [x | x <- xs, x `mod` 2 == 1]
        evens = [x | x <- xs, x `mod` 2 == 0]