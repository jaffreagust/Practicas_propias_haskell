data Nat = Cero | Succ Nat deriving Show

--1)a)--

--Succ :: Nat->Nat

--1)b) --

int2Nat :: Int -> Nat

int2Nat 0 = Cero
int2Nat x = Succ (int2Nat (x-1))


--1)c)--
sumaNat Cero x = x
sumaNat (Succ(y)) x = Succ(sumaNat y x)


--1)d)--
nat2Int Cero = 0
nat2Int (Succ(x)) = 1+ nat2Int x


--EJERCICIO 2--

--2)a)--
data Arb = E | H Int | N Arb Arb deriving Show

data Cmd = L|R deriving Show

--N:: Arb->Arb->Arb

--2)b)-- 
selec :: [Cmd] -> Arb->Arb

selec [] arb = arb
selec [L] (N l r) = l
selec [R] (N l r) = r
selec (L:xs) arb = selec xs (selec [L] arb)
selec (R:xs) arb = selec xs (selec [R] arb)




--Ejemplo : (N (N (H 3) (H 2)) (H 3))
insertarEnList a [] = []
insertarEnList a (x:xs) = (a:x):(insertarEnList a xs)


enum (E) = [[]]
enum (H _) = [[]]
enum (N izq E) = insertarEnList (L) (enum izq)
enum (N E der) = insertarEnList (R) (enum der)
enum (N izq der) = (insertarEnList (L) (enum izq)) ++ (insertarEnList (R) (enum der)) 

