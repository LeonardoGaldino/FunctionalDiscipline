import Test.QuickCheck

import Data.List
import Data.Char
import System.Random
import Control.Monad

{- Questão 1 -}

data Tree = Node Int Tree Tree | Empty deriving (Show)

emptyTree :: Tree
emptyTree = Empty

insertNode :: Tree -> Int -> Tree
insertNode Empty val = Node val Empty Empty
insertNode (Node curVal left right) val
    | val < curVal = Node curVal (insertNode left val) right
    | val > curVal = Node curVal left $ insertNode right val
    | otherwise = Node curVal left right

treeFromList :: [Int] -> Tree
treeFromList = foldl (\acc v -> insertNode acc v ) emptyTree

generateInt :: Int -> Int -> Gen Int
generateInt left right = choose (left, right)

generateListN :: Int -> Gen [Int]
generateListN n = sequence $ take n $ repeat $ generateInt 0 1000

generateList :: Gen [Int]
generateList = (generateInt 0 100) >>= generateListN

generateTree :: Gen Tree
generateTree = generateList >>= (\v -> return $ treeFromList v)

countInnerNodes :: Tree -> Int
countInnerNodes Empty = 0
countInnerNodes (Node _ Empty Empty) = 0
countInnerNodes (Node _ left right) = 1 + countInnerNodes left + countInnerNodes right

countLeaves :: Tree -> Int
countLeaves Empty = 0
countLeaves (Node _ Empty Empty) = 1
countLeaves (Node _ left right) = countLeaves left + countLeaves right

countNodes :: Tree -> Int
countNodes tree = countLeaves tree + countInnerNodes tree

countHeight :: Tree -> Int
countHeight Empty = 0
countHeight (Node _ left right) = 1 + max (countHeight left) (countHeight right)

instance Arbitrary Tree where
    arbitrary = generateTree

prop_checkLeavesCount :: Tree -> Bool
prop_checkLeavesCount tree = countLeaves tree <= (countInnerNodes tree) + 1

prop_checkMaxNodes :: Tree -> Bool
prop_checkMaxNodes tree = countNodes tree <= (2 ^ ((countHeight tree) + 1)) - 1

{- Questão 2 -}

class Indexable a where
    (!#) :: a -> Int -> Int

data Set = Set [Int] deriving Show

instance Eq Set where
    (Set xs) == (Set ys) = xs == ys

emptySet :: Set
emptySet = Set []

instance Indexable Set where
    (!#) (Set xs) idx = xs !! idx

setInsert :: Set -> Int -> Set
setInsert (Set []) val = Set [val]
setInsert (Set xs) val = Set (takeWhile (<val) xs ++ (val : (dropWhile (<=val) xs)))

setFromList :: [Int] -> Set
setFromList xs = foldl (\acc v -> setInsert acc v) emptySet xs

setUnion :: Set -> Set -> Set
setUnion (Set xs) (Set ys) = setFromList $ xs ++ ys

setIntersection :: Set -> Set -> Set
setIntersection (Set xs) (Set ys) = Set [v | v <- xs, v `elem` ys]

setIsSubset :: Set -> Set -> Bool
setIsSubset (Set xs) (Set ys) = all (`elem` xs) ys

setDifference :: Set -> Set -> Set
setDifference (Set xs) (Set ys) = Set [v | v <- xs, not $ v `elem` ys]

generateSet :: Gen Set
generateSet = generateList >>= (\v -> return $ setFromList v)

instance Arbitrary Set where
    arbitrary = generateSet

prop_checkSetUnionCommutativity :: Set -> Set -> Bool
prop_checkSetUnionCommutativity set1 set2 = setUnion set1 set2 == setUnion set2 set1

prop_checkSetIntersectionCommutativity :: Set -> Set -> Bool
prop_checkSetIntersectionCommutativity set1 set2 = setIntersection set1 set2 == setIntersection set2 set1

prop_checkSetUnionIdentity :: Set -> Bool
prop_checkSetUnionIdentity set1 = setUnion set1 emptySet == set1

prop_checkSetSelfUnionIdentity :: Set -> Bool
prop_checkSetSelfUnionIdentity set1 = setUnion set1 set1 == set1

prop_checkSetUnionBelonging :: Set -> Set -> Bool
prop_checkSetUnionBelonging set1 set2 = setIsSubset union set1 && setIsSubset union set2
    where
        union = setUnion set1 set2

prop_checkSetDifferenceExclusion :: Set -> Set -> Bool
prop_checkSetDifferenceExclusion set1 set2 = setIntersection s1_s2 set2 == emptySet && setIntersection s2_s1 set1 == emptySet
    where
        s1_s2 = setDifference set1 set2
        s2_s1 = setDifference set2 set1

{- Questão 3 -}

type Edge = (Int, Int)
data Graph = Graph [Edge] deriving Show

instance Arbitrary Graph where
    arbitrary = generateGraph

descendants :: Graph -> Int -> [Int]
descendants (Graph edges) v = map snd $ filter ((==v) . fst) edges

bfs :: Graph -> Int -> Int -> [Int]
bfs g init 0 = []
bfs g init depth = nub $ nexts ++ [v | next <- nexts, v <- bfs g next (depth-1)]
    where
        nexts = descendants g init

dfsJoin :: [Int] -> [[Int]] -> [Int]
dfsJoin [] (xs:xss) = xs ++ dfsJoin [] xss
dfsJoin xs [] = xs
dfsJoin (x:xs) (xss:xsss) = x:xss ++ dfsJoin xs xsss

dfs :: Graph -> Int -> Int -> [Int]
dfs g init 0 = [init]
dfs g init depth = dfsJoin nexts explore
    where
        nexts = descendants g init
        explore = [dfs g next (depth-1) | next <- nexts]
        
generateEdges :: Int -> Int -> Gen [Edge]
generateEdges maxNodes maxEdges = shuffle [(from, to) | from <- [0..maxNodes-1], to <- [0..maxNodes-1]] >>= (return . take maxEdges)

generateGraph :: Gen Graph
generateGraph = do
    generateInt 0 maxNodes >>= \numNodes
        -> generateInt 0 (numNodes*numNodes) >>= \numEdges
            -> generateEdges numNodes numEdges >>= \edges
                -> return $ Graph edges
                where
                    maxNodes = 30

{- Se o elemento 2 é encontrado partindo do elemento 0, com profundidade máxima de 5 usando DFS -}
{- Então o elemento 2 também deve ser encontrando com os mesmos parâmetros usando uma BFS -}
prop_checkGraphSearch :: Graph -> Property
prop_checkGraphSearch g = targetElement `elem` dfs g initialNode maxDepth ==> targetElement `elem` bfs g initialNode maxDepth
    where
        targetElement = 2
        initialNode = 0
        maxDepth = 5
    
{- Questão 4 -}

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

instance Functor Expr where
    fmap f (Var v) = Var $ f v

    fmap _ (Val v) = (Val v)

    fmap f (Add expr1 expr2) = Add (f <$> expr1) (f <$> expr2)

instance Applicative Expr where
    pure v = Var v

    (Var f) <*> (Var v) = f <$> (Var v)
    (Var f) <*> (Val v) = Val v
    (Var f) <*> (Add expr1 expr2) = Add (f <$> expr1) (f <$> expr2)

    (Val v) <*> expr = Val v

    (Add f1 f2) <*> (Val v) = Val v
    (Add f1 f2) <*> expr = Add (f1 <*> expr) (f2 <*> expr)

instance Monad Expr where
    return v = Var v

    (Val v) >>= f = (Val v)
    (Var v) >>= f = f v
    (Add expr1 expr2) >>= f = Add (expr1 >>= f) (expr2 >>= f)

{-
    Podemos ver, neste exemplo, que o operador bind (>>=) aplica a função provida
    à expressão. Ao achar uma expressão do tipo Add, 
    aplica o operador bind, com a mesma função, duas vezes: uma para cada expressão.
    Ao achar uma expressão do tipo Var, que pode ser visto como um dos casos base, 
    aplica a função ao valor do Var. Ao achar uma expressão do tipo Val, a função não pode
    ser aplicada por divergência do parâmetro da função e do tipo que Val contém, logo, retorna Val com o mesmo valor.
-}
bindExample :: Expr Int
bindExample = Add (Val 12) (Add (Var 'a') (Var 'b')) >>= ((\v -> Var v) . (+5) . ord)

{- Questão 5 -}

type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
    fmap g st = do
        v <- st
        S (\s -> (g v, s))

instance Applicative ST where
    pure x = S (\s -> (x, s))
    stf <*> stv = do
        v <- stv
        f <- stf
        pure $ f v 

instance Monad ST where
    st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')
