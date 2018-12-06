module Task5_1 where

import Todo(todo)

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec

index :: DList a -> Int -> a
index DNil _ = error "404"
index (DCons l c r) i | i == 0 = c
                      | otherwise = index r $ i-1

goToTheBeginning :: DList a -> DList a
goToTheBeginning DNil = DNil
goToTheBeginning list @(DCons DNil _ _) = list
goToTheBeginning (DCons l _ _) = goToTheBeginning l

insertAt :: DList a -> Int -> a -> DList a
insertAt list index value = append (goToTheBeginning list) value DNil index

removeAt :: DList a -> Int -> DList a
removeAt list index = remove (goToTheBeginning list) DNil index

remove DNil _ _ = DNil
remove list @(DCons _ _ DNil) _ ind
    | ind == 0 = DNil
    | otherwise = list
remove list @(DCons l c r @(DCons _ cr rr)) super ind
    | ind == 0 = end
    | otherwise = mya
    where mya = DCons super c (remove r mya $ ind-1) 
          end = DCons l cr (makeLeft rr end)

append list @(DNil) el super ind
    | ind == 0 = end
    | otherwise = error "404"
    where end = DCons super el (makeLeft list end)
append list @(DCons l c r) el super ind
    | ind == 0 = end
    | otherwise = mya
    where mya = DCons super c (append r el mya $ ind-1)
          end = DCons super el (makeLeft list end)

makeLeft DNil _ = DNil
makeLeft (DCons l c r) left = 
    let new = DCons left c (makeLeft r new)
    in new
