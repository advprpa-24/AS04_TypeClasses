module TypeClasses.Person where

data Person = MkPerson { nr:: Int, email :: String }

instance Show Person where
  show :: Person -> String
  show (MkPerson nr email) = "Person " ++ show nr ++ " " ++ email   

instance Eq Person where
  (==) :: Person -> Person -> Bool
  (==) (MkPerson nr1 _) (MkPerson nr2 _) = nr1 == nr2

instance Ord Person where
  compare :: Person -> Person -> Ordering
  compare (MkPerson nr1 _) (MkPerson nr2 _) = compare nr1 nr2
