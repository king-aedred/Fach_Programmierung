
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
