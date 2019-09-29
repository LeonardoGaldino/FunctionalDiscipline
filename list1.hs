import Data.List

{- For importing <$> operator -}
import Control.Applicative 

{- Questão 3 -}
eps :: Double
eps = 0.000000001

searchSqrt :: [Double] -> Double
searchSqrt (x:y:xs)
    | abs(x-y) < eps = y
    | otherwise = searchSqrt (y:xs)

sqroot :: Int -> Double
sqroot n = searchSqrt approxs
    where 
        next' = \x -> (x + (fromIntegral n :: Double)/x) / 2
        approxs = iterate next' 1.0

{- Questão 4 -}

{- Tree Implementation -}
data Tree a = Node a (Tree a) (Tree a) | NullNode

instance (Show a) => Show (Tree a) where
    show (Node val left right) = "Node " ++ show val ++ " (" ++ show left ++ ")" ++ " (" ++ show right ++ ")"
    show NullNode = "-"

{- Make Tree a functor -}
instance Functor Tree where
    fmap _ NullNode = NullNode
    fmap f (Node val left right) = Node (f val) (fmap f left) (fmap f right)

emptyTree :: Tree a
emptyTree = NullNode

newNode :: a -> Tree a
newNode v = Node v NullNode NullNode

insertNode :: (Ord a) => a -> Tree a -> Tree a
insertNode val NullNode = newNode val
insertNode newVal (Node val left right)
    | newVal == val = Node val left right
    | newVal > val = Node val left (insertNode newVal right)
    | otherwise = Node val (insertNode newVal left) right

searchNode :: (Ord a) => a -> Tree a -> Tree a
searchNode val NullNode = NullNode
searchNode searchVal (Node val left right)
    | searchVal == val = Node val left right
    | searchVal > val = searchNode searchVal right
    | otherwise = searchNode searchVal left

treeToList :: Tree a -> [a]
treeToList NullNode = []
treeToList (Node val left right) = val:((treeToList left) ++ (treeToList right))

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList xs = foldr insertNode NullNode $ reverse xs

{- Set Implementation -}

data Set a = SetInstance (Tree a)

instance (Show a, Ord a) => Show (Set a) where
    show (SetInstance tree) = show $ (sort . treeToList) tree

{- Make Set a functor -}
instance Functor Set where
    fmap f (SetInstance tree) = SetInstance $ f <$> tree

emptySet :: Set a
emptySet = SetInstance emptyTree

addElement :: (Ord a) => Set a -> a -> Set a
addElement (SetInstance tree) val = SetInstance (insertNode val tree)

removeElement :: (Ord a) => Set a -> a -> Set a
removeElement (SetInstance tree) val = SetInstance $ (treeFromList . filter (/= val) . treeToList) tree

hasElement :: (Ord a) => Set a -> a -> Bool
hasElement (SetInstance tree) val = val `elem` treeToList tree

setFromList :: (Ord a) => [a] -> Set a
setFromList xs = SetInstance $ treeFromList xs

setToList :: Set a -> [a]
setToList (SetInstance tree) = treeToList tree

unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet (SetInstance tree1) (SetInstance tree2) = SetInstance $ treeFromList $ treeToList tree1 ++ treeToList tree2

isSubset :: (Ord a) => Set a -> Set a -> Bool
isSubset (SetInstance tree) set = all (hasElement set) (treeToList tree)

setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (SetInstance tree1) set = setFromList [el | el <- treeToList tree1, hasElement set el]

setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (SetInstance tree1) set = setFromList [el | el <- treeToList tree1, not $ hasElement set el]

setSize :: Set a -> Int
setSize set = length $ setToList set