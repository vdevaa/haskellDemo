-- Vijay Deva (11762589)

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

-- Implement merge sorted lists in ascending order
mergeAscending :: Ord a => [a] -> [a] -> [a]
--- Base cases:
-- if the first list gets to empty, we can just deal with the second list
mergeAscending [] sl = sl
mergeAscending fl [] = fl

mergeAscending fl@(x:xs) sl@(y:ys)
    | x < y = x : mergeAscending xs sl
    | otherwise = y : mergeAscending fl ys

-- Implement merge sorted lists in descending order

mergeDescending :: Ord a => [a] -> [a] -> [a]
-- Base cases:
mergeDescending [] sl = sl
mergeDescending fl [] = fl

mergeDescending fl@(x:xs) sl@(y:ys)
    | x > y = x : mergeDescending xs sl
    | otherwise = y : mergeDescending fl ys

-- Implementing merge sort algorithim
mergeSort :: Ord a => [a] -> [a]
-- -- Base case:
mergeSort [] = []
mergeSort [x] = [x]

-- Use mergeAcending on split left and right
mergeSort xs = mergeAscending(mergeSort l) (mergeSort r)
    where
        (l,r) = splitLists xs
        -- for base cases
        splitLists [] = ([],[])
        splitLists [f] = ([f],[])
        splitLists (f:l:r) =
            let (ls, rs) = splitLists r
            in (f:ls, l:rs)
        

-- Implement Insertion Sort
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = recursiveInsert x (insertionSort xs)
    where
        recursiveInsert x [] = [x]
        recursiveInsert x ls@(y:ys)
            | x < y = x : ls
            | otherwise = y : recursiveInsert x ys -- resivly call to find the right spot


-- takeList Testing
testTakeListNorm = if takeList 4 [1,2,3,4,5] == [1,2,3,4]
    then "test passed"
    else "test failed"

testTakeListEdgeZero = if takeList 0 [1,2,3,4,5] == []
    then "test passed"
    else "test failed"

testTakeListEdgeMax = if takeList 10 [1,2,3,4,5] == [1,2,3,4,5]
    then "test passed"
    else "test failed"

testTakeListEdgeEmpty = if takeList 0 ([] :: [Int]) == []
    then "test passed"
    else "test failed"

-- splitByCondition Testing
testSplitByConditionEven = if splitByCondition even [1,2,3,4,5] == ([1,3,5], [2,4])
    then "test passed"
    else "test failed"

testSplitByConditionOdd = if splitByCondition odd [1,2,3,4,5] == ([2,4], [1,3,5])
    then "test passed"
    else "test failed"

testSplitByConditionEmpty = if splitByCondition even [] == ([],[])
    then "test passed"
    else "test failed"

testSplitByConditionNoMatch = if splitByCondition even [1,3,5] == ([1,3,5], [])
    then "test passed"
    else "test failed"

-- zipLists Testing
testZipListsNorm = if zipLists [1,2] [3,4] == [(1,3), (2,4)]
    then "test passed"
    else "test failed"

testZipListsEdgeEmptyFirst = if zipLists ([] :: [Int]) [1,2] == []
    then "test passed"
    else "test failed"

testZipListsEdgeEmptySecond = if zipLists [1,2] ([] :: [Int]) == []
    then "test passed"
    else "test failed"

testZipListsEdgeEmptyBoth = if zipLists ([] :: [Int]) ([] :: [Int]) == []
    then "test passed"
    else "test failed"

testInterleaveNorm = if interleaveLists [1,3,5] [2,4,6,7,8] == [1,2,3,4,5,6]
    then "test passed"
    else "test failed"

testInterleaveEmptyFirst = if interleaveLists ([] :: [Int]) [1,3,5] == []
    then "test passed"
    else "test failed"

testInterleaveEmptySecond = if interleaveLists [1,3,5] ([] :: [Int]) == []
    then "test passed"
    else "test failed"

testInterleaveEmptyBoth = if interleaveLists ([] :: [Int]) ([] :: [Int]) == []
    then "test passed"
    else "test failed"

-- mergeAscending Testing
testMergeAscendingNorm = if mergeAscending [1,3,5] [2,4,6] == [1,2,3,4,5,6]
    then "test passed"
    else "test failed"

testMergeAscEmpty = if mergeAscending ([] :: [Int]) [1,2,3] == [1,2,3]
    then "test passed"
    else "test failed"

-- mergeDescending Testing
testMergeDescNorm = if mergeDescending [8,6,4] [7,5,3] == [8,7,6,5,4,3]
    then "test passed"
    else "test failed"

testMergeDescEmpty = if mergeDescending ([] :: [Int]) [3,2,1] == [3,2,1]
    then "test passed"
    else "test failed"

-- mergeSort Testing
testMergeSortNorm = if mergeSort [3,2,1] == [1,2,3]
    then "test passed"
    else "test failed"

testMergeSortEmpty = if mergeSort ([] :: [Int]) == []
    then "test passed"
    else "test failed"

-- insertionSort Testing
testInsertionSortNorm = if insertionSort [3,1,2] == [1,2,3]
    then "test passed"
    else "test failed"

testInsertionSortEmpty = if insertionSort ([] :: [Int]) == []
    then "test passed"
    else "test failed"


runTests = do
    putStrLn testTakeListNorm
    putStrLn testTakeListEdgeZero
    putStrLn testTakeListEdgeMax
    putStrLn testTakeListEdgeEmpty
    putStrLn testSplitByConditionEven
    putStrLn testSplitByConditionOdd
    putStrLn testSplitByConditionEmpty
    putStrLn testSplitByConditionNoMatch
    putStrLn testZipListsNorm
    putStrLn testZipListsEdgeEmptyFirst
    putStrLn testZipListsEdgeEmptySecond
    putStrLn testZipListsEdgeEmptyBoth
    putStrLn testInterleaveNorm
    putStrLn testInterleaveEmptyFirst
    putStrLn testInterleaveEmptySecond
    putStrLn testInterleaveEmptyBoth
    putStrLn testMergeAscendingNorm
    putStrLn testMergeAscEmpty
    putStrLn testMergeDescNorm
    putStrLn testMergeDescEmpty
    putStrLn testMergeSortNorm
    putStrLn testMergeSortEmpty
    putStrLn testInsertionSortNorm
    putStrLn testInsertionSortEmpty
