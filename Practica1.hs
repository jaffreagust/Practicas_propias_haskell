module Practica0 where

import Data.List

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


{-
4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
a) (Int -> Int) -> Int
b) Int -> (Int -> Int)
c) (Int -> Int) -> (Int -> Int)
d) Int -> Bool
e) Bool -> (Bool -> Bool)
f) (Int,Char) -> Bool
g) (Int,Int) -> Int
h) Int -> (Int,Int)
i) a -> Bool
j) a -> a
-}


{-
5) Definir las siguientes funciones usando listas por comprensión:

a) 'divisors', que dado un entero positivo 'x' devuelve la
lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

b) 'matches', que dados un entero 'x' y una lista de enteros descarta
de la lista los elementos distintos a 'x'

c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
donde 0 <= a, b, c, d <= 'n'

(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
'xs' sin elementos repetidos
unique :: [Int] -> [Int]
-}



{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

{-
7) Sin usar funciones definidas en el
preludio, defina recursivamente las siguientes funciones y
determine su tipo más general:

a) 'suma', que suma todos los elementos de una lista de números

b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario

c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario

d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales

e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado

f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados

g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes

h) 'orden', que dada una lista de pares de números, devuelve
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
