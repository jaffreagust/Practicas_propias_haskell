import Data.List
import System.IO

{-
También queremos usar Haskell para programas interactivos
Leer del teclado y mostrar resultados por la pantalla
Los programas en Haskell son computaciones puras. (No tienen efectos laterales)

efecto lateral de una computacion: mostrar el resultado de un computo

Los programas interactivos poseen efectos laterales

El tipo de las acciones que retorna el tipo a es IO a:

IO Char: el tipo de acción que retorna el tipo caracter

IO (): el tipo de acción que no retorna valor

La anotación "do" combina una secuencia de acciones


act:: IO (Char,Char)
act = do x<-getChar
         getChar
         y<-getChar
         return (x,y)

Las expresiones de tipo x<-m son llamadas generadores, del lado derecho hay una monada
, por ejemplo en y<- getChar es una monada, pero x<- xs en listas por comprension no hay monada



Haskell ejecuta las operaciones de IO
IO no puede ser ddefinido en Haskell, es una primitiva 

-}
act:: IO (Char,Char)
act = do x<-getChar
         getChar
         y<-getChar
         return (x,y)


getLine' :: IO String
getLine' = do x<-getChar
              if x == '\n' then return []
                           else do xs<- getLine'
                                   return (x:xs)

putStr' :: String-> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs





putStrLn' xs = do putStr' xs
                  putChar' '\n'
                             


--TAREA--
{-
Ahorcado multijugador

cada jugador elige dos palabras secretas, una para que vos adivines y se ingresan los dos nombres

nombrej1
nombrej2
palabra para nombrej2
palabra para nombrej1
 
empieza el juego
-}
