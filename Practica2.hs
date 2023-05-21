import Data.List
--EJERCICIO 1

--Sin record
data Color = RGB Int Int Int deriving Show

mezclar (RGB x y z) (RGB x1 y1 z1) = (RGB (div (x+x1) 2) (div (y+y1) 2) (div (z+z1) 2) ) 

--Con record
data Color_reg = Color_reg{
                 r::Int,
                 g::Int,
                 b::Int
                }deriving Show


mezclaralt (Color_reg r g b) (Color_reg r1 g1 b1) = (Color_reg (div (r+r1) 2) (div (g+g1) 2) (div (b+b1) 2))


--EJERCICIO 2--

--Con data y registro

data Linea = Linea {
            charac::[Char],
            pos::Int
            }deriving Show

vacia = (Linea "" 0)

--Notese que no se piensa si pos es mayor al largo de la cadena,
-- ya que ese no es inconveniente del calculo principal, de la logica de la funcion
moverIzq (Linea a pos) | (pos == 0) = (Linea a pos)
                       | otherwise = (Linea a (pos-1)) 

moverDer (Linea a pos) | (pos == length a) = (Linea a pos)
                       | otherwise = (Linea a (pos+1)) 

moverIni (Linea a pos) = (Linea a 0)

moverFin (Linea a pos) = (Linea a (length a))

--Insertar v1--
f (char::Char) a pos= (take pos a) ++ [char] ++ (drop pos a)
insertarv1 char (Linea a pos) = (Linea (f char a pos) (pos+1))

--Insertar v2--
insertarv2 char (Linea a 0) = (Linea (char : a) 1)
insertarv2 char (Linea a pos) = (Linea (insertar char a pos) (pos+1))

insertar char a 1 = char:a
insertar char (x:xs) pos = x:insertar char xs (pos-1)

--Borrar v1--
borrarv1 (Linea a pos) = (Linea ((take (pos-1) a) ++ (drop pos a)) (pos-1))

borrarv2 (Linea a pos) = (Linea (bor a pos) (pos-1))

bor (x:xs) 1 = xs  
bor (x:xs) pos = x: bor xs (pos-1)  

--EJERCICIO 2 CON TYPES

type Cadena = [Char]
type Cursor = Int

--"Holaquetal" = ("aloh","quetal")
type Linea1 = (Cadena,Cadena)


--Mover izq--
moverIzq1 ("",xs) = ("",xs)
moverIzq1 ((x:xs),ys) = (xs,x:ys)

--Mover der--
moverDer1 (xs,"") = (xs,"")
moverDer1 (xs,(y:ys)) = (y:xs,ys)

--MoverIni--
moverIni1 (xs,ys) = ("",reverse xs ++ ys)

--MoverFin--
moverFin1 (xs,ys) = (reverse ys ++ xs , "")

--Insertar--
insertar1 char (xs,ys) = (char:xs,ys)

--Borrar --
borrar1 ((x:xs),ys) = (xs,ys) 


--EJERCICIO 3--
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show

headCL (CUnit x) = x
headCL (Consnoc x _ _) = x

tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x (EmptyCL) y) = CUnit y
tailCL (Consnoc x xs z) = (Consnoc (headCL xs) (tailCL xs) z)

isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit (CUnit x)= True
isCUnit _ = False

reverseCL (EmptyCL) = EmptyCL
reverseCL (CUnit x) = CUnit x
reverseCL (Consnoc x xs y) = (Consnoc y (reverseCL xs) x)

--Ejercicio 3c--
--Ejercicio 3d--
--Ejercicio 3e--

--EJERCICIO 4--