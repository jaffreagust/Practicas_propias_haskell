import Data.List
import Data.Char
import Data.Maybe

--Ejercicio 1 B--
class Array t where
    ini ::Int -> t (Maybe a)
    insertar ::Int -> a  -> t (Maybe a) -> t (Maybe a)
    view :: Int -> t (Maybe a) -> Maybe a
    size :: t (Maybe a) -> Int 


instance Array [] where
    ini 0 = []
    ini x = Nothing : (ini (x-1))

    view _ [] = Nothing
    view 0 (y:ys) = y
    view x (y:ys) = view (x-1) ys  

    insertar 1 x (y:ys) = (Just x):ys
    insertar k x (y:ys) = if (k > size (y:ys)  || k < 1) then (y:ys) else  y: (insertar (k-1) x ys)

    size [] = 0
    size (y:ys) = 1 + size ys 