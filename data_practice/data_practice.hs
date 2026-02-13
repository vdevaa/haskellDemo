import Distribution.Simple.Utils (xargs)
type Name = String


addSalutation:: Name -> String
addSalutation n = "Dr." ++ n

type Point = (Double, Double)

type Student = (Int, String, String)

---newtype USD = USD Double
---newtype INR = INR Double

---convert(INR x) = USD $ x * 90

---convertToINR(USD x) = INR $ x / 90


data Currency = USD Double 
    | INR Double 
    | EUR Double
    deriving (Show, Eq)


convert :: Currency -> Currency
convert(USD x) = USD x
convert(INR x) = USD $ x / 90
convert(EUR x) = USD $ x * 0.9

-- instance Num Currency where
--     (+) (USD x) (USD y) = USD $ x + y
--     (+) (USD x) (INR y) = USD $ x + (y / 90)
--     (+) (USD x) (EUR y) = USD $ x + (y * 0.9)
--     (+) (INR x) (USD y) = USD $ (x / 90) + y
--     (+) (INR x) (INR y) = USD $ (x / 90) + (y / 90)
--     (+) (INR x) (EUR y) = USD $ (x / 90) + (y * 0.9)
--     (+) (EUR x) (USD y) = USD $ (x * 0.9) + y
--     (+) (EUR x) (INR y) = USD $ (x * 0.9) + (y / 90)
--     (+) (EUR x) (EUR y ) = USD $ (x * 0.9 ) + (y * 0.9)

data Temperature = Ferenheit Double
    | Celcius Double
    | Kelvin Double
    deriving (Show, Eq)

data Shape = Circle Double
    | Rectangle Double Double
    | Square Double
    deriving (Show, Eq)

c = Circle 10
r = Rectangle 10 20
s = Square 10

area (Square s) = s * s
area (Circle r) = pi * r * r
area (Rectangle l b) = l * b



area' shape = case shape of
    Circle r -> pi * r * r
    Square s -> s * s
    Rectangle l h -> l * h


totalCost shape lcost = 
    let materialCost = case shape of
            Circle r -> (area shape) * 5.0
            _ -> (area shape) * 4.0
        laborCost = case lcost of
            USD amt -> amt
            INR amt -> amt / 90
            EUR amt -> amt * 0.9
    in USD $ materialCost + laborCost


-- --- Represents customer with name, age, and email
-- data Customer = Customer String Int String
--     deriving Show

-- customer = Customer "Vijay" 20 "vijay@test.com"

-- getCustomerName (Customer name _ _) = name


data Customer = Customer {
    customerName :: String,
    customerAge :: Int,
    customerEmail :: String
} deriving (Show, Eq)

customer = Customer {
    customerName = "Vijay",
    customerAge = 20,
    customerEmail = "vijay#test.com"
}

customer1 = customer {
    customerEmail = "alex#test.com"
}

-- data IBox = IBox Int deriving Show
-- data SBox = SBox String deriving Show

data Box a = Box a deriving Show

data MColor = MRed | MGreen | MBlue

printColor::MColor -> String
printColor MRed = "its Red"
printColor MGreen = "its Green"
printColor MBlue = "its Blue"

data BTRee a = Empty
    | Node a (BTRee a) (BTRee a)
    deriving Show

btree = (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))

treeSize Empty = 0
treeSize (Node _ left right) = 1 + (treeSize left) + (treeSize right)


treeDepth Empty = 0
treeDepth (Node _ left right) = 1 + max (treeDepth left) (treeDepth right)


-- Find eleemnt in tree

treeFind _ Empty = False
treeFind x (Node y left right)
    | x == y = True
    | otherwise = treeFind x left || treeFind x right


type Peg = Char
type Move = (Int, Peg, Peg)

hanoi 0 _ _ _ = []
hanoi n source destination aux = 
    hanoi (n-1) source aux destination ++
    [(n, source, destination)] ++
    hanoi (n-1) aux destination source