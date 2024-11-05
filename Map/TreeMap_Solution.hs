{-# LANGUAGE InstanceSigs #-}
module Map.TreeMap_Solution (
  Map, empty, insert, fromList, fromListMerge, toList, lookup, insertOrMerge
) where
import Data.List (intercalate)
import Prelude hiding (lookup)

todo :: a
todo = error "TODO"

data Map k v 
    = Empty 
    | Branch (Map k v) k v (Map k v)
    deriving Eq


-- Creates an empty Map.
empty :: Ord k => Map k v 
empty = Empty 


-- Returns the value stored under the given key k.
lookup :: Ord k => k -> Map k v -> Maybe v
lookup _ Empty = Nothing
lookup k (Branch l k' v' r) 
  | k == k' = Just v'
  | k < k'  = lookup k l 
  | k > k'  = lookup k r

-- Inserts the given key-value pair into the given Map.
-- Smaller keys go to the left, larger keys to the right.
-- If there is already an entry with the given key, 
-- the original value is replaced with the new value.
insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert k v Empty = Branch Empty k v Empty 
insert k v (Branch l k' v' r) 
  | k == k' = Branch l k v r
  | k < k'  = Branch (insert k v l) k' v' r 
  | k > k'  = Branch l k' v' (insert k v r)



-- Inserts the given key-value pair into the given Map.
-- If there is already an entry with the given key,
-- the Semigroup of the values is used to combine the existing value
-- with the new value.
insertOrMerge :: (Ord k, Semigroup v) => k -> v -> Map k v -> Map k v
insertOrMerge k v Empty = Branch Empty k v Empty 
insertOrMerge k v (Branch l k' v' r) 
  | k == k' = Branch l k (v <> v') r
  | k < k'  = Branch (insertOrMerge k v l) k' v' r 
  | k > k'  = Branch l k' v' (insertOrMerge k v r) 

insert' :: (Ord k) => k -> v -> Map k v -> Map k v
insert' = insertWith const

insertOrMerge' :: (Ord k, Semigroup v) => k -> v -> Map k v -> Map k v
insertOrMerge' = insertWith (<>)

-- (toInsertValue -> currentValue -> resulting value)
insertWith :: (Ord k) => (v -> v -> v)-> k -> v -> Map k v -> Map k v
insertWith f k v Empty = Branch Empty k v Empty 
insertWith f k v (Branch l k' v' r) 
  | k == k' = Branch l k (f v v') r
  | k < k'  = Branch (insertWith f k v l) k' v' r 
  | k > k'  = Branch l k' v' (insertWith f k v r) 


-- Creates a Map from a list of pairs.
-- Use foldr or foldl and the insert function to implement fromList.
-- When inserting mappings with the same key, the last one should overwrite previous ones.
-- E.g. the Map created using `fromList [("a", 1), ("a", 2)]` should only contain `("a", 2)`.
fromList :: Ord k => [(k,v)] -> Map k v
fromList = foldl (flip $ uncurry insert) Empty


-- Creates a Map from a list of pairs.
-- Use foldr and the insertOrMerge function to implement fromListMerge.
fromListMerge :: (Ord k, Semigroup v) => [(k,v)] -> Map k v
fromListMerge = foldr (uncurry insertOrMerge) Empty


-- Returns the list of mappings.
toList :: Map k v -> [(k,v)]
toList Empty = []
toList (Branch l k v r) = toList l ++ (k,v) : toList r


-- Produces a pretty formatted list of mappings:
-- k1 -> v1
-- k2 -> v2
-- ... 
-- Hint: Using toList and Data.List.intercalate can simplify this task.
instance (Show k, Show v) => Show (Map k v) where
  show :: Map k v -> String
  show m = intercalate "\n" (map (\(k,v) -> show k ++ " -> " ++ show v) (toList m))


-- Two Maps can be combined if the keys are equatable
-- and if there is a Semigroup instance for the values.
-- For entries with the same keys, the values should be combined using (<>).
-- Again, use the insertOrMerge function.
instance (Ord k, Semigroup v) => Semigroup (Map k v) where
  (<>) :: (Ord k, Semigroup v) => Map k v -> Map k v -> Map k v
  l <> r = foldr (uncurry insertOrMerge) r (toList l)


-- Creates an empty Map (easy). 
instance (Ord k, Semigroup v) => Monoid (Map k v) where
  mempty :: (Ord k, Semigroup v) => Map k v
  mempty = Empty
