{-# LANGUAGE InstanceSigs #-}
module Map.TreeMap (
  Map, empty, insert, insertOrMerge, fromList, toList, fromListMerge, lookup
) where
import Data.List (intercalate)
import Prelude hiding (lookup)

todo :: a
todo = error "TODO"


-- The Map data type to map keys k to values v.
data Map k v 
    = Empty 
    | Branch (Map k v) k v (Map k v)
    deriving Eq


-- Creates an empty Map.
empty :: Ord k => Map k v 
empty = todo 


-- Inserts the given key-value pair into the given Map.
-- Smaller keys go to the left, larger keys to the right.
-- If there is already an entry with the given key, 
-- the original value is replaced with the new value.
insert :: (Ord k) => k -> v -> Map k v -> Map k v
insert = todo 


-- Returns the value stored under the given key k.
-- Spec: get k (insert k v m) == Just v
lookup :: Ord k => k -> Map k v -> Maybe v
lookup = todo


-- Inserts the given key-value pair into the given Map.
-- If there is already an entry with the given key,
-- the Semigroup of the values is used to combine the existing value
-- with the new value.
insertOrMerge :: (Ord k, Semigroup v) => k -> v -> Map k v -> Map k v
insertOrMerge = todo 


-- Creates a Map from a list of pairs.
-- Use foldr or foldl and the insert function to implement fromList.
-- When inserting mappings with the same key, the last one should overwrite previous ones.
-- E.g. the Map created using `fromList [("a", 1), ("a", 2)]` should only contain `("a", 2)`.
fromList :: Ord k => [(k,v)] -> Map k v
fromList = todo


-- Creates a Map from a list of pairs.
-- Use foldr and the insertOrMerge function to implement fromListMerge.
fromListMerge :: (Ord k, Semigroup v) => [(k,v)] -> Map k v
fromListMerge = todo


-- Returns the list of mappings.
toList :: Map k v -> [(k,v)]
toList = todo


-- Produces a pretty formatted list of mappings:
-- k1 -> v1
-- k2 -> v2
-- ... 
-- Hint: Using toList and Data.List.intercalate can simplify this task.
instance (Show k, Show v) => Show (Map k v) where
  show :: Map k v -> String
  show = todo


-- Two Maps can be combined if the keys are equatable
-- and if there is a Semigroup instance for the values.
-- For entries with the same keys, the values should be combined using (<>).
-- Again, use the insertOrMerge function.
instance (Ord k, Semigroup v) => Semigroup (Map k v) where
  (<>) :: (Ord k, Semigroup v) => Map k v -> Map k v -> Map k v
  (<>) = todo


-- Creates an empty Map (easy). 
instance (Ord k, Semigroup v) => Monoid (Map k v) where
  mempty :: (Ord k, Semigroup v) => Map k v
  mempty = todo
