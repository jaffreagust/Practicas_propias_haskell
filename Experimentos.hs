import Data.List 

serie xs = [take i xs| i<- [0..length xs]]

borrarUltimo (x:[]) = []
borrarUltimo (x:xs) = x:borrarUltimo xs

serie' [] = [[]]
serie' xs = serie' (borrarUltimo xs) ++ [xs]


ror xs 0 = xs
ror (x:xs) n =ror (xs++[x]) (n-1)


upto n m | n>m = []
         | n<=m = n: upto (n+1) m


euler n = sum [x | x<- [0..n-1] , x `mod` 3 == 0 || x `mod` 5 == 0]

repn n = [(\x -> n) x| x<- [1..n] ]
expandir xs = [y | x<- xs, y<- repn x]