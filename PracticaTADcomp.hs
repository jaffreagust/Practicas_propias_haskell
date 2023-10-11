import Data.List
import Data.Char
import Data.Maybe

--Ejercicio 2--
type Bag a = [(a,Int)]

vacio = []

insertar a [] = [(a,1)]
insertar a ((x,y):xs) = if (a==x) then (a,y+1) : xs else (x,y):insertar a xs

borrar a [] = []
borrar a ((x,z):xs) = if a == x then (x,z-1):xs else (x,z):borrar a xs

uniona bag1 [] = bag1
uniona [] bag2 = bag2
uniona ((x,0):xs) bag2 = uniona xs bag2
uniona ((x,y):xs) bag2 = uniona ((x,y-1):xs) (insertar x bag2)

pertenece x [] = False
pertenece x ((w,y):ws) = if x == w then True else pertenece x ws 


data Inv a = H | N (Inv a) (Maybe (a,Int)) (Inv a) deriving(Show)

new 0 = H
new 1 = N H Nothing H
new n = if (mod n 2 == 0) then N (new (div n 2)) Nothing (new (div (n-1) 2))
                          else N (new (div (n-1) 2)) Nothing (new (div (n-1) 2))


pert id H = False
pert id (N l x r) = if(fst(fromJust x) == id) then True else pert id l || pert id r


collect id (N H Nothing H) = N H (Just(id,1)) H
collect id (N l@(N l2 Nothing r2) Nothing r) = N (collect id l) Nothing r
collect id (N l Nothing r) = if (pert id l) then N (collect id l) Nothing r else N l (Just(id,1)) r 
collect id (N l x1@(Just (x,n)) r) = if(x == id) then N l (Just (x,n+1)) r else if (pert id l) then N (collect id l) x1 r  else N l x1 (collect id r) 

type Columnas a=[Maybe a]
type Matriz a=[Columnas a]

newc 0 = []
newc n = Nothing : newc (n-1)

new1 0 n = []
new1 n1 n = newc n : new1 (n1-1) n 


sizef [] = 0
sizef (xs:xss) = 1 + sizef xss

size [] = (0,0)
size m@(xs:xss) = (sizef m , length xs)



setc 1 a (x:xs) = Just a : xs
setc n a (x:xs) = x:setc (n-1) a xs

set 1 j a (xs:xss) = (setc j a xs) : xss
set i j a m@(xs:xss) = if(i<= fst(size m) && j <= snd(size m)) then xs:(set (i-1) j a xss) else m

getc 1 (x:xs) = x
getc n (x:xs) = getc (n-1) xs

get 1 j (xs:xss) = (getc j xs)
get i j m@(xs:xss) = if(i<= fst(size m) && j <= snd(size m)) then (get (i-1) j xss) else Nothing

