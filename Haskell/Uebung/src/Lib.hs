-- Übung 1
-- Aufgabe 1
sum3 :: Int -> Int -> Int -> Int
sum3 x y z = x + y + z

-- Aufgabe 2
-- a
fac :: Int -> Int
fac 0 = 1
fac n = n * fac(n-1)

-- b
sumFacs :: Int -> Int -> Int
sumFacs n m
    | n>=0 && n<=m = fac n + sumFacs (n+1) m
    | otherwise = 0

-- Aufgabe 3
fib :: Int -> Int
fib n
    | n>=3 = fib(n-2) + fib(n-1)
    | otherwise = 1


-- Übung 2
-- Aufgabe 1
bincoeff :: Int -> Int -> Int
bincoeff n k
    | n>=k = fac n `div` (fac k * fac (n-k))
    | otherwise = 0
    where
    fac 0 = 1
    fac n = n * fac(n-1)


-- Aufgabe 2
-- a
prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod(xs)

-- b
rev :: [Int] -> [Int]
rev (x:xs) = rev(xs) ++ [x]

-- c
excl :: Int -> [Int] -> [Int]
excl _ [] = []
excl a (x:xs)
    |a==x = excl a xs
    |otherwise = x:excl a xs 

-- d
isOrd :: [Int] -> Bool
isOrd [] = True
isOrd [x] = True
isOrd (x:y:xs)
    |x<=y = isOrd (y:xs)
    |otherwise = False

-- e
merge :: [Int] -> [Int] -> [Int]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    |x>=y = y: merge (x:xs) ys
    |otherwise = x: merge xs (y:ys)

-- f
fibs :: [Int]
fibs = f 0
    where f i = fib (i) : f(i+1)


-- Übung 3
-- Aufgabe 1
-- a
isPrefix :: String -> String -> Bool
isPrefix [] ys = True
isPrefix xs [] = False
isPrefix (x:xs) (y:ys) = x==y && isPrefix xs ys

--b
countPattern :: String -> String -> Int
countPattern [] [] = 1
countPattern x [] = 0
countPattern x (y:ys)
    |isPrefix x (y:ys) = countPattern x ys +1
    |otherwise = countPattern x ys

--Aufgabe 2
data BinTree = Branch Int BinTree BinTree | Nil
    deriving (Show)

-- a
mytree :: BinTree
mytree = Branch 0
                Nil
                (Branch 3
                        (Branch 1 Nil Nil)
                        (Branch 5 Nil Nil))

-- b                        
equal :: BinTree -> BinTree -> Bool
equal Nil Nil = True
equal _ Nil = False
equal Nil _ = False
equal (Branch x lx rx) (Branch y ly ry) = x==y && equal lx ly && equal rx ry

--c
insert :: BinTree -> [Int] -> BinTree
insert t [] = t
insert t (x:xs) = insert (insertOne t x) xs

insertOne :: BinTree -> Int -> BinTree
insertOne Nil x = Branch x Nil Nil
insertOne (Branch x l r) i
    |i<=x = Branch x (insertOne l i) r
    |otherwise = Branch x l (insertOne r i)

--d
unwind :: BinTree -> [Int]
unwind t = hilfunwind [t]

hilfunwind :: [BinTree] -> [Int]
hilfunwind [] = []
hilfunwind ((Branch x l r):xs) = [x] ++ hilfunwind (xs ++[l,r])
hilfunwind (Nil:xs) = hilfunwind xs

-- Übung4
-- Aufgabe 1
f:: [Int] -> Int
f x = foldr (*) 1 (map (\i -> i^2) (filter even x))

-- Aufgabe 2
foldleft :: (a -> b -> a) -> a -> [b] -> a
foldleft f a [] = a
foldleft f a (x:xs) = foldleft f (f a x) xs

-- Aufgabe 3
data Tree a = Node a [Tree a]
    deriving (Show)

--a
t :: Tree Char
t = Node 'a' [
        Node 'b' [Node 'c' [], Node 'd' []],
        Node 'e' [Node 'f' []],
        Node 'g' []]
--b
oddTree :: Tree a -> Bool
oddTree (Node _ []) = True
oddTree (Node _ xs) = odd (length xs) && foldr (&&) True (map oddTree xs)

--c
preOrder :: Tree a -> [a]
preOrder (Node a []) = [a]
preOrder (Node x xs) = foldleft (++) [x] (map preOrder xs)