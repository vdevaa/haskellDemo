--- Implement drop

-- How to debug

import Debug.Trace (trace)


dropLs n xs| trace (" n = " ++ show n ++ " xs = " ++ show xs) False = undefined
dropLs n [] = []
dropLs n ls@(x:xs)
    | n > 0 = dropLs (n - 1) xs
    | otherwise = ls

-- Implement take

takeList ::Int -> [a] -> [a]
takeList n [] = []
takeList n ls@(x:xs)
    | n > 0 = x : takeList (n - 1) xs
    | otherwise = []

-- Implement splitByCondition

splitByCondition :: (a -> Bool) -> [a] -> ([a], [a])
splitByCondition _ [] = ([],[])
splitByCondition n (x:xs)
    | n x = (ys, x:zs)
    | otherwise = (x:ys, zs)
    where (ys, zs) = splitByCondition n xs

-- Implement zipLists

zipLists :: [a] -> [b] -> [(a,b)]
zipLists [] _ = []
zipLists _ [] = []
zipLists (x:xs) (y:ys) = (x,y) : zipLists xs ys

-- Implement interleaveLists
interleaveLists :: [a] -> [a] -> [a]
interleaveLists [] _ = []
interleaveLists _ [] = []
interleaveLists (x:xs) (y:ys) = x : y : interleaveLists xs ys


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