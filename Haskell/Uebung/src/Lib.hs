module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "Hello World"

-- Übung 2
-- Aufgabe 1
bincoeff :: Int -> Int -> Int
bincoeff n k
    | n>=k = fac n 'div' (fac k * fac (n-k))
    | otherwise = 0
    where
    fac 0 = 1
    fac n = n* fac(n-1)


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
    |x<=y isOrd (y:xs)
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


-- Zusatz

cl :: Int -> Int
cl 1 = 1
cl n = cl(cnext n) +1

cnext :: Int -> Int
cnext n
    |even n = n 'div' 2
    |otherwise = 3*n +1

cmax :: Int -> Int
cmax 1 = 1
cmax n = maximum (take n (collatz n))

ck :: [Int] -> Int
ck x = maximum x

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n n: (collatz (cnext n))

-- Übung 3
-- Aufgabe 1
-- a
isPrefix :: Stirng -> String -> Bool
isPrefix [] ys_= True
isPrefix xs [] = False
isPrefix (x:xs) (y:ys) = x==y && isPrefix xs ys

--b
countPattern :: String -> String -> Int
countPattern [] [] = 1
countPattern x [] = 0
countPattern x (y:ys)
    |isPrefix x (y:ys) = countPattern x ys +1
    |otherwise countPattern x ys

--Aufgabe 2
data BinTree = Branch Int BinTree BinTree | Nil
 deriving [Show]

-- a
mytree :: BinTree
mytree = Branch 0
                NIL
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
hilfunwind ((Branch x l r):xs) = x. hilfunwind (xs ++[l,r])
hilfunwind (Nil:xs) = hilfunwind xs