module Practica0 where

import Data.List
import Data.Char

{-
1) Los siguientes códigos tienen errores, cargar el archivo 20.Practica.0.hs en el interprete de Haskell
GHCi, leer los mensajes de error y corregirlos hasta que el archivo se cargue correctamente.
-}

-- a)
regla b = case b of
    True -> "Quedate en Casa"
    False -> "Qudate en Casa"

-- b)--"case" es una palabra reservada, se cambio a la palabra "caso"--
caso [] =  []
caso [x] = []
caso (x:y:xs) =  y : caso (x:xs)


-- c)--"map" es reservada tambien, entonces se cambio a "map_alt"
map_alt f []        =  []
map_alt f (x:xs)     =  f x : map_alt f xs

-- d)--error en el tipo de dato, siendo listNumeros::[int], se arregla colocando un numero
listNumeros = 1 : 2 : 3 : [] 

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys

-- f)-- el operador del map debe estar entre parentesis  y haskell asocia a la izquierda entonces "map (+x) tail xs" = "(map (+x) tail) xs" lo cual da error--
addToTail x xs = map (+x) (tail xs)

-- g)--En haskell la aplicacion tiene mayor precedencia que cualquier otro operador--
-- al hacer "head . sort xs" primero resuelve sort llevando a un error al intentar juntar head con (sort xs)--
listmin xs = (head.sort) xs

-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs

{-
2. Definir las siguientes funciones y determinar su tipo:-}

--a) five, que dado cualquier valor, devuelve 5
five :: Num a => p -> a
five _ = 5

--b) apply, que toma una función y un valor, y devuelve el resultado de
--aplicar la función al valor dado
apply :: (a -> b) -> a -> b
apply f x = f x 

--c) identidad, la función identidad
identidad :: Int -> Int
identidad x = x

--d) first, que toma un par ordenado, y devuelve su primera componente
first :: (a,b) -> a
first (x,y) = x

--e) derive, que aproxima la derivada de una función dada en un punto dado
derive :: Integral a =>(a -> a) -> a -> a
derive f x = ((f x) - (f 0)) `div` x

--f) sign, la función signo
sign :: Int -> Int
sign x | (x<0) = -1
       | (x>0) = 1
       | otherwise = 0

{-sign :: Int -> String
sign x | (x<0) = "Negative"
       | (x>0) = "Positive"
       | otherwise = "Zero"-}

--g) vabs, la función valor absoluto (usando sign y sin usarla)
vabswith :: Int -> Int
vabswithout :: Int -> Int --TIPO DE DATO ORD SIGNIFICA QUE ES UN DATO ORDENABLE , ES DECIR COMPARABLE, SE MENCIONA EN CASO DE PATTERN MACHINE O IF--
vabswithout x = if x<0 then -x else x
vabswith x = sign x *x

--h) pot, que toma un entero y un número, y devuelve el resultado de
--elevar el segundo a la potencia dada por el primero
pot :: Num a => Int -> a -> a --Tuve que poner Num a => porque interpreta como literal a 1
pot 0 y = 1
pot x y = y * (pot (x-1) y)

--i) xor, el operador de disyunción exclusiva
xor :: Bool -> Bool -> Bool
xor x y | (x==y) = False
        | (x||y) = True
        | otherwise = False

--j) max3, que toma tres números enteros y devuelve el máximo entre ellos
max3 ::Ord a=> a->a->a->a
max3 x y z = if x>=y && x>=z then x else if y>=x && y>=z then y else z

--k) swap, que toma un par y devuelve el par con sus componentes invertidas
swap::(a,b) -> (b,a)
swap (x,y) = (y,x)

{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}
bisiest :: Int->Bool
bisiest x = if (x `mod` 4) /= 0 then False 
                              else if (x `mod` 100) /= 0 then True 
                                                       else if (x `mod` 400) ==0 then True else False  



--4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
example4b x y = x*y
example4e (x::Bool) (y::Bool)= x&&y
example4ee x = case x of
       True -> False
       False-> True

--a) (Int -> Int) -> Int
fun4a :: (Int->Int)->Int
fun4a f= f 4 

fun4aa :: (Int->Int)->Int
fun4aa g = 2*(g 3) 

--b) Int -> (Int -> Int)
fun4b :: Int->(Int->Int)
fun4b x =  example4b x

fun4bb:: Int->(Int->Int)
fun4bb y = pot (y+0)


--c) (Int -> Int) -> (Int -> Int)
fun4c :: (Int->Int) -> (Int->Int)
fun4c vabswith = vabswithout

fun4cc :: (Int->Int)->(Int->Int)
fun4cc pot y = pot 10

--d) Int -> Bool
fun4d (x::Int) = x>0

fun4dd (x::Int) = x>10 
--e) Bool -> (Bool -> Bool)
fun4e (u::Bool) = example4e u
fun4ee (x::Bool) = example4ee  

--f) (Int,Char) -> Bool
fun4f (x::Int,y::Char) = x==ord(y)
fun4ff (x::Int,y::Char) = chr(x)==y


--g) (Int,Int) -> Int
fun4g (x::Int,y::Int) = x+y

fun4gg (x::Int,y::Int) = x-y

--h) Int -> (Int,Int)
fun4h (x::Int) = (x,x)
fun4hh (x::Int) = (x,2*x)


--i) a -> Bool
fun4i (x::a) = x>0
fun4ii (x::a) = mod x 2 == 0

--j) a -> a
fun4j (x::a) = x
fun4jj (i::a) = i


--5) Definir las siguientes funciones usando listas por comprensión:

{-a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)-}
divisors x | (x<0) = []
           | (x>=0) = [div x y |y<- [1..x], mod x y ==0]

{-b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'-}
matches x ys = [y| y<-ys,x==y]

{- c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'-}

cuadrupla n = [(a,b,c,d)| a<-[0..n],b<-[0..n],c<-[0..n],d<-[0..n], (pot 2 a)+(pot 2 b) == (pot 2 c)+(pot 2 d)]


{-(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
unique :: [Int] -> [Int]
-}
--aux toma una lista y un elemento y devuelve True si existe otro elemento despues en la lista
aux e xs =not ([x|x <-xs , e==x] ==[])

--dropl toma un indice y una lista y retorna todos los elementos siguientes al indice
dropl n xs = [x|(x,i)<- zip xs [1..],i>n]

--unique toma una lista y retorna todos los elementos tales que no esten despues en la lista (not (aux))
unique xs = [u | (i,u)<- zip [1..] xs, not(aux u (dropl i xs))]


{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}


scalarproduct xs ys = sum [x*y|(i,x)<-zip [1..] xs ,(u,y)<-zip [1..] ys,i==u]

scalarproduct1 [] [] = 0
scalarproduct1 (x:xs) (y:ys) = x*y+scalarproduct1 xs ys

{-
7) Sin usar funciones definidas en el
preludio, defina recursivamente las siguientes funciones y
determine su tipo más general:-}

--a) 'suma', que suma todos los elementos de una lista de números
suma [] = 0
suma (x:xs) = x+suma(xs)

{-b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario-}
alguno [] = False
alguno (x:xs) = x || alguno xs


{-c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario -}
todos [] = True
todos (x:xs)= x && todos xs


{-d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales-}


{-e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado-}

restos x [] = []
restos x (y:ys) = [mod y x] ++ restos x ys 

{-f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados-}
cuadrados [] = []
cuadrados (x:xs) = [x*x] ++ cuadrados xs

{-g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes-}
altlenght [] = 0
altlenght (x:xs) = 1+altlenght xs

longitudes [] = []
longitudes (xs:xss) = [altlenght xs] ++ longitudes xss 

{-h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda

i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares

j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)

k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}

{-
8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
ver su definición en https://hoogle.haskell.org/
-}
