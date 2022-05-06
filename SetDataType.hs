
data Set a = NullSet | Node (Set a) a (Set a) deriving Show


toList :: Set a -> [a]
toList NullSet = []
toList (Node tLeft x tRight) = toList tLeft ++ [x] ++ toList tRight


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x : removeDuplicates xs


sort :: Ord a => [a] -> [a]
sort [] = []
sort(x:xs) = sort [a | a <- xs, a <= x] ++ [x] ++ sort [b | b <- xs, b > x]

-- auxiliary function to make balanced binary search tree
makeTree :: [a] -> Set a
makeTree [] = NullSet
makeTree xs = Node (makeTree tLeft) x (makeTree tRight)
    where
      n = length xs
      (tLeft, x:tRight) = splitAt (n `div` 2) xs


-- fromList [2,1,1,4,5] => {2,1,4,5}
fromList :: Ord a => [a] -> Set a
fromList xs = makeTree $ (removeDuplicates $ sort xs)



-- test if two sets have the same elements.
instance (Ord a) => Eq (Set a) where
  s1 == s2 = toList s1 == toList s2


-- the empty set
empty :: Set a
empty = NullSet


-- Set with one element
singleton :: a -> Set a
singleton e = Node NullSet e NullSet


-- insert an element of type a into a Set
-- make sure there are no duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert e NullSet = singleton e
insert e (Node tLeft x tRight)
    | e < x = Node (insert e tLeft) x tRight -- inserts into left subtree
    | e > x = Node tLeft x (insert e tRight) -- inserts into right subtree
    | e == x = (Node tLeft x tRight)


-- join two Sets together
union :: (Ord a) => Set a -> Set a -> Set a
union NullSet set = set
union set NullSet = set
union (Node tLeft x tRight) setTwo
    | member x setTwo = union tLeft $ union tRight setTwo
    | otherwise = insert x $ union tLeft $ union tRight setTwo


-- return the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection setOne setTwo = setfoldr (\x set -> if (member x setTwo) then insert x set else set) setOne NullSet


-- all the elements in Set A *not* in Set B,
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference setOne setTwo = setfoldr (\x set -> if not (member x setTwo) then insert x set else set) setOne NullSet


-- is element *a* in the Set?
member :: (Ord a) => a -> Set a -> Bool
member e NullSet = False
member e (Node tLeft x tRight)
    | e == x = True
    | e < x = member e tLeft -- searches in left subtree
    | e > x = member e tRight -- searches in right subtree


-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality NullSet = 0
cardinality (Node tLeft x tRight) = 1 + cardinality tLeft + cardinality tRight


setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f NullSet = NullSet
setmap f (Node tLeft x tRight) = Node (setmap f tLeft) (f x) (setmap f tRight)


setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr f NullSet base = base
setfoldr f (Node tLeft x tRight) base = setfoldr f tLeft base'
      where
        base'  = f x base''
        base'' = setfoldr f tRight base


-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: Set a -> Set (Set a)
powerSet NullSet = NullSet
powerSet set = makeTree $ map makeTree $ powerSetList $ toList set
    where
      powerSetList :: [a] -> [[a]]
      powerSetList xs = []: foldr fn [] xs
          where fn x acc = [x]: fmap (x:) acc ++ acc


-- cartesian product of two sets
cartesian :: Set a -> Set b -> Set (a, b)
cartesian setOne setTwo = makeTree [(x,y) | x <- xs, y <- ys]
      where
        xs = toList setOne
        ys = toList setTwo


-- partition the set into two sets, with
-- all elements that satisfy the predicate on the left,
-- and the rest on the right
partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition fn set = (setfoldr (\x set -> if fn x then insert x set else set) set NullSet, setfoldr (\x set -> if not (fn x) then insert x set else set) set NullSet)
    where
      insert :: a -> Set a -> Set a
      insert e NullSet = singleton e
      insert e (Node tLeft x tRight) = Node (insert e tLeft) x tRight
