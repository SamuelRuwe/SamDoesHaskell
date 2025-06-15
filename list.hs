-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
data List a = Empty | Cons {listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)
