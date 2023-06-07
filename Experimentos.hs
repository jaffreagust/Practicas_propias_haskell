import Data.List 
import Data.Char

type Rank = Int
data Heap a= Em| Nod Rank a (Heap a) (Heap a) deriving (Show)


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



--Calcular el numero de nodos en un nivel especifico de un arbol binario
data BTS a = H | N (BTS a) a (BTS a) deriving (Show)
numnod _ H = 0
numnod 0 (N l x r) = 1
numnod y (N l x r) = numnod (y-1) l + numnod (y-1) r 

numnod' _ H = []
numnod' 0 (N l x r) = [x]
numnod' y (N l x r) = numnod' (y-1) l ++ numnod' (y-1) r 


alturarb h H = [h] 
alturarb h (N l x r) = alturarb (h+1) l ++ alturarb (h+1) r 

checkBalanceBST H = True
checkBalanceBST arb@(N l x r) = (minimum (alturarb 0 arb) +1) >= (maximum (alturarb 0 arb))


data Color = R|B deriving (Show)
data RBT a = E | Nodo Color (RBT a) a (RBT a) deriving (Show)

eq R R = True
eq B B = True
eq _ _ = False

makeBlack E = E 
makeBlack (Nodo R l x r)  = (Nodo B l x r)
makeBlack nodo = nodo  

inorderRBT E = []
inorderRBT (Nodo _ l x r) = inorderRBT l ++ [x] ++ inorderRBT r 

fromListRBT [] = E 
fromListRBT xs = if (truncate(logBase 2 (fromIntegral(length xs))) `mod` 2 == 0) then makeBlack(fromListRBT' B xs)
                                                                   else fromListRBT' R xs

fromListRBT' _ [] = E 
fromListRBT' c  xs = let l = length xs 
                         u = div l 2
                         m = xs!!u
                         zs = take u xs
                         ys = drop (u+1) xs
                         (t1,t2) =  (fromListRBT' B zs, fromListRBT' B ys)
                         (t3,t4) =  (fromListRBT' R zs, fromListRBT' R ys)
                         in if (eq c R) then Nodo B t1 m t2
                                       else Nodo R t3 m t4 


balanceadoRBT arb = fromListRBT (inorderRBT arb)


alturaNegra E = 0
alturaNegra (Nodo B l _ r) = max (1+ alturaNegra l) (1+alturaNegra r)
alturaNegra (Nodo R l _ r) = max (alturaNegra l) (alturaNegra r)

maximunRBT (Nodo _ l x E ) = x 
maximunRBT (Nodo _ l x r) = maximunRBT r

minimunRBT (Nodo _ E x r ) = x 
minimunRBT (Nodo _ l x r) = minimunRBT l

checkBSTRBT E = True
checkBSTRBT (Nodo _ E x E) = True
checkBSTRBT (Nodo _ E x r) =  (minimunRBT r) >= x && checkBSTRBT r 
checkBSTRBT (Nodo _ l x E) = (maximunRBT l) <= x && checkBSTRBT l 
checkBSTRBT (Nodo _ l x r) = (maximunRBT l) <= x && checkBSTRBT l && (minimunRBT r) >= x && checkBSTRBT r 

checkRBT' E = True
checkRBT' (Nodo _ E x E) = True
checkRBT' (Nodo R (Nodo R _ y _) x _ ) = False
checkRBT' (Nodo R _ x (Nodo R _ y _ )) = False
checkRBT' arb@(Nodo _ E x r) = alturaNegra r == 0 && (checkBSTRBT arb) && checkRBT' r 
checkRBT' arb@(Nodo _ l x E) = alturaNegra l == 0 &&(checkBSTRBT arb) && checkRBT' l 

checkRBT' arb@(Nodo _ l x r) = (alturaNegra l == alturaNegra r) && (checkBSTRBT arb) && checkRBT' l && checkRBT' r


checkRBT (Nodo R _ _ _) = False
checkRBT arb = checkRBT' arb


borrarUltimo []  = []
borrarUltimo [x] = []
borrarUltimo (x:xs) = x: borrarUltimo xs

ylist  x [] = []
ylist  x ((z,y):ys) = if x == z then y: ylist x ys
                                else ylist x ys


onlyone x [] = []
onlyone x ((z,y):ys) = if x == z then  onlyone x ys
                                 else (z,y) : onlyone x ys

collect [] = []
collect ((x,y):xs) = (x, ylist x ((x,y):xs)) : collect(onlyone x xs)


serie [] = [[]]
serie xs = serie(borrarUltimo xs) ++ [xs]



paresIguales [] = True
paresIguales (x:xs) | ((length (delete x xs)) `mod` 2 /= 0) = False
                    | otherwise = paresIguales (delete x xs)


ror xs 0 = xs
ror (x:xs) n = ror (xs++[x]) (n-1)

upto n m | n==m = [m]
         | otherwise = n: upto (n+1) m

--Modelo superior--
eco' _ _ [] = []
eco' n 0 (x:xs) = eco' (n+1) (n+1) xs
eco' n m xs = head(xs) : eco' n (m-1) xs

eco xs = eco' 1 1 xs


cambios xs = [u| (x,y) <- zip [0..] xs , (u,v) <- zip [0..] xs, u==x-1 && v /= y]

oblongoNum = [x*(x+1)| x<- [1..]]

repetir n x = [x| y<- [1..n]]

ecoprime xs = foldr (++) []  [repetir y x| (x,y) <- zip xs [1..] ]


abundantes = [x | x<- [1..] , x< (divisores x)]
divisores x = foldr (+) 0 [y | y<- [1..x-1], x `mod` y == 0]

euler n = sum([x | x <- [1..n-1] , x `mod` 3 == 0 || x `mod` 5 == 0])

expandir xs = foldr (++) [] [repeater x | x <- xs]
repeater x = [ x | y<- [0..x-1]]


--Ej 3  1.1--
funa x y = (True &&)
funb True  = (3>)
func c = chr(ord(c)+1)
fund x f = if f x then [x,x,x]
                    else [1,2,3]


fune (x:xs) f =  reverse (f x)

funf ((y:xs):xss) g = if (g y) then xs else []


fung (a,b,c) = True
funh (a,b,c ) x =  if x +10 == 20 then c else c
funi (a,b,c) x = if a==b && b == c && sum(x,10) > 10 then a else b
            
 
--METODO 1--

minL (Nod _ x Em Em) = x 
minL (Nod _ x Em r) = min x (minL r)
minL (Nod _ x l Em) = min x (minL l)
minL (Nod _ x l r) = min x (min (minL l) (minL r))


--Agregue la condicion de que el rango del padre tiene que ser igual al rango del hijo derecho más 1, asi se evitan problemas en el rango
checkLHeap' Em = True
checkLHeap' (Nod c x Em Em) = c == 1
checkLHeap' t@(Nod c x l Em) = let m = minL t
                                   in x == m && checkLHeap' l && c == 1
checkLHeap' t@(Nod c x Em r) = let m = minL t
                                   in x == m && checkLHeap' r && c == (rank r)+1 
checkLHeap' t@(Nod c x l r) = let m = minL t
                                  lc = rank l
                                  rc = rank r
                                  in x == m && lc >= rc && checkLHeap' l && checkLHeap' r && c==rc+1



--METODO 2--

findMin Em = 0
findMin (Nod _ x _ _) = x

checkLHeap Em = True
checkLHeap (Nod c x Em Em) = c==1
checkLHeap (Nod c x l Em) = x<findMin(l) && c==1 && checkLHeap l 
checkLHeap (Nod c x Em r) = x<findMin(r) && c == (rank r) +1 && checkLHeap r 
checkLHeap (Nod c x l r) = x<findMin(l) && x<findMin(r) && c==(rank r) +1 && rank l >= rank r  &&checkLHeap l && checkLHeap r 



fromList'  [] = Em
fromList' xs = let zs = map (\x-> Nod 1 x Em Em) xs
                   pares [] = []
                   pares [x] = [x]
                   pares (x:y:ys) = (merge x y) : pares ys
                   g [] = Em
                   g [x] = x 
                   g ys = g (pares ys) 
                   in g zs

