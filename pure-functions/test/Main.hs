module Main (main) where
import Test.HUnit (runTestTT, Test(..), Testable(..), assertEqual)
import MyLib (add, sub, fac)


-- Where we can change things

addTest :: Test
addTest = TestCase(assertEqual "testing adding 1 and 2" 3 (add 1 2))

subTest :: Test
subTest = TestCase(assertEqual "testing subtracting 2 from 5" 3 (sub 5 2))

facTest :: Test
facTest = TestCase(assertEqual "testing factorial of 1" 1 (fac 1))

tests = TestList [addTest, subTest, facTest]

-- WHere changes end


main :: IO ()
main = do
    _ <- runTestTT tests
    return ()
