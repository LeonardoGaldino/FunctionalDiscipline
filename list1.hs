import Data.List
import Data.Char

{- For importing <$> operator -}
import Control.Applicative 


{- Questão 1 -}

let2int :: Char -> Int
let2int =  (+ (- ord 'a')) . ord

int2let :: Int -> Char
int2let = chr . (+ ord 'a')

shift :: Int -> Char -> Char
shift key value = (int2let . (\v -> v `mod` 26) . (+key) . let2int) value

encode :: Int -> String -> String
encode key xs = unwords $ map (\wd -> shift key <$> wd) $ words xs

percent :: Int -> Int -> Float
percent n1 n2 = ((fromIntegral n1) :: Float) / ((fromIntegral n2) :: Float)

freqs :: String -> [Float]
freqs xs = [percent (counter letter) len | letter <- ['a'..'z']]
    where
        len = length xs
        lowered = map toLower xs
        counter letter = (length . filter (== letter)) lowered

freqTable :: [Float]
freqTable = [0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015, 0.06094, 
    0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749, 0.07507, 0.01929, 0.00095,
    0.05987, 0.06327, 0.09056, 0.02758, 0.00978, 0.02360, 0.00150, 0.01974, 0.00074]

chisqr :: [Float] -> [Float] -> Float
chisqr [] [] = 0
chisqr (x:xs) (y:ys) = ((x-y)*(x-y)/y) + chisqr xs ys

rotate :: Int -> [a] -> [a]
rotate offset xs = (reverse . take complement . reverse) xs ++ take offset' xs
        where
            offset' = offset `mod` length xs
            complement = length xs - offset'

crack :: String -> String
crack inp = encode (-lowestIndex) inp
    where
        table = freqs inp
        possibilities = [(n, chisqr (rotate n table) freqTable) | n <- [0..25]]
        lowestIndex = (fst . foldr1 (\v acc -> if snd v < snd acc then v else acc)) possibilities

main :: IO ()
main = getLine >>= (\v -> putStrLn (encode 3 v) >> (putStrLn $ show (crack (encode 3 v))))


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