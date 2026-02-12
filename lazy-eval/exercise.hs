import Debug.Trace
generateNumber n = [1..n]

odd' n | trace ("called with " ++ show n ++ "\n") False = undefined
odd' n = n `mod` 2 /= 0

filterOdds xs = filter odd' xs

takeOdds = take 5 (filterOdds (generateNumber 100))