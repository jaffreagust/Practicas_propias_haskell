import Data.List
import Data.Char
--Ejercicio 1--
--a)--
borrarUltimo [] = []
borrarUltimo [x] = []
borrarUltimo (x:xs) = x:borrarUltimo xs

--b)--
ylist x xs = [y| (z,y) <- xs ,z == x]

onlyonex x xs = [(z,y) | (z,y)<- xs , not (z==x)]

collect [] = []
collect ((x,y):xs) = (x, ylist x ((x,y):xs)) : collect( onlyonex x xs)

--c)--
tomar 0 xs = [take 0 xs]
tomar n xs = [take n xs] ++ tomar (n-1) xs

serie [] = [[]]
serie xs =serie (take ((length xs) - 1) xs) ++ [take (length xs) xs]

--d)--
descartar x [] = []
descartar x (y:xs) = if x == y then descartar x xs
                               else y:descartar x xs

paresIguales [] = True
paresIguales (x:xs) | ((length (descartar x xs)) `mod` 2 /= 0) = False
                    | otherwise = paresIguales (descartar x xs)

--e)--

isosceles x y z = if x==y || x==z || z==y then True else False

--f)--
ror [] n = []
ror xs 0 = xs
ror (x:xs) n = (ror (xs++[x]) (n-1))

--g)--
upto n m = if n>m then [] 
                  else n : upto (n+1) m

--h)--
repeatLetter 0 x = ""
repeatLetter n x = x : repeatLetter (n-1) x

eco' n [] = []
eco' n (x:xs) = repeatLetter n x ++ eco' (n+1) xs

eco xs =  eco' 1 xs

--EJERCICIO 2--
--a)--
cambios xs = [i| (u,y)<- zip [0..] xs, (i,x)<- zip [0..] xs, u==i-1 && x/=y]

--b)--
oblongos n = [(x*(x+1))| x<- [1..n]]


--c)--
abundantes :: Int->[Int]
divisores :: Int-> [Int]
divisores x = [i| i<- [1..x-1] , mod x i ==0]
abundantes n = [x | x<- [0..n], (foldr (+) 0 (divisores x)) > x]

--d)--
ecoprime xs =foldr (++) [] [repeatLetter i x| (i,x)<- zip [1..] xs]

--e)--
euler n = sum [x| x<- [0..n-1], mod x 5 ==0 || mod x 3 == 0 ]

--f)--
repeatNumber [] y = []
repeatNumber (x:xs) y = y: repeatNumber xs y

expandir xs = foldr (++) [] [repeatNumber [0..y] y | y<-xs ]


--EJERCICIO 3--

suma (x::Int) (y::Int) = x+y

--a)-- 
fun3a (f :: Int->Int)  = not

--b)--
f :: Int->Bool
f _ = False
fun3b True = f

--c)--
fun3c char = chr(ord(char)+1)

--d)--

fun3d (x::Int) (daux::Int->Bool)  = [x]

--e)--
aux3e (x::a) = [x| x<-[0..x]]
fun3e (x:xs::[a]) (f::a->[b]) = f x

--f)--
fun3f (xs:xss) y = [x | x<- xs , y x ]

--g)--
fun3g (x,y,z) = x>y

--h)--
fun3h (a ,b ,c) (x::Int) = c

--i)--

fun3i ((x,y,z)::(a,a,a)) (u::Int) = x

--EJERCICIO 4--

--a)--
foo1 p = if p then (p &&) else (p &&)
--Bool->Bool->Bool

--b)--
foo2 x y z = x (y z )
-- (x:: t1->t2) (y:: t3->t1) (z :: t3) ||||| (t1->t2)->(t3->t1)->t3 => t2

--c)--
--foo3 x y z  =  x y z
-- t1->t2->t3 => t1 t2 t3

--d)--
 foo4 x y z = x y : z


--e)--
 foo5 x y z = x : y z
--f)--
 foo6 x y z = x ++ y z
--g)-- 
foo7 a b = if b a then head a else [ ]
--h)-- 
foo8 a b = if b a then a else [ ]
--i)-- 
foo9 a b = if b a then head (:a) else (:[ ])
