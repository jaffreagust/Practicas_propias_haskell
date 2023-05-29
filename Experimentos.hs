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

type Rank = Int
data Heap a= Em| Nod Rank a (Heap a) (Heap a) deriving (Show)


checkLHeap Em = True
checkLHeap (Nod _ _ Em Em ) = True
checkLHeap (Nod _ a1 Em r@(Nod _ a2 l1 r1)) = a1<a2
checkLHeap (Nod _ a1 l@(Nod _ a2 l1 r1) Em) = a1<a2 
checkLHeap (Nod _ a1 l@(Nod rank1 a2 l1 r1) r@(Nod rank2 a3 l2 r2)) = a1<a2 && a1<a3 && rank1>= rank2 && checkLHeap l && checkLHeap r


fromList [] = Em
fromList xs = let hs = map (\x-> Nod 1 x Em Em) xs
                  pares [] = []
                  pares [x] = [x]
                  pares (x:y:ys) = merge x y : pares ys
                  g [] = Em
                  g[h] = h
                  g ys = g (pares ys)
                  in g hs

--Fusion entre dos Leftist Heap--
merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 Em = h1
merge Em h2 = h2
merge h1@(Nod _ x a1 b1) h2@(Nod _ y a2 b2) =
    if x <= y then makeH x a1 (merge b1 h2)
              else makeH y a2 (merge h1 b2)

--Extrae el rango
rank :: Heap a -> Rank
rank Em = 0
rank (Nod r _ _ _) = r

--Consulta el rango de los dos nodos
-- Si es mas grande el rango de b se incrementa en uno por la raiz añadida
-- el mas grande pasara a ser la espina izquierda (se menciona primero), y la menor será la derecha
makeH x a b = if rank a >= rank b then Nod (rank b + 1) x a b
                                  else Nod (rank a + 1) x b a