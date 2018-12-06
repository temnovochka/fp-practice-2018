module Task6 where

import Todo(todo)

-- value, left, right, parent
data LinkedTree a = Pusto | Branch a (LinkedTree a) (LinkedTree a) (LinkedTree a)

find :: (Ord a) => LinkedTree a -> a -> Bool
find Pusto _ = False
find (Branch v l r p) a
    | a == v = True
    | a > v = find r a
    | otherwise = find l a

goToTheBeginning :: LinkedTree a -> LinkedTree a
goToTheBeginning Pusto = Pusto
goToTheBeginning tree @(Branch _ _ _ Pusto) = tree
goToTheBeginning (Branch _ _ _ p) = goToTheBeginning p

insert' Pusto v prev = Branch v Pusto Pusto prev
insert' tree @(Branch v l r p) a prev
    | a == v = tree
    | a > v = right
    | otherwise = left
    where right = Branch v (setParent l right) (insert' r a right) prev
          left = Branch v (insert' l a left) (setParent r left) prev

insert :: (Ord a) => LinkedTree a -> a -> LinkedTree a
insert tree a = insert' (goToTheBeginning tree) a Pusto
   
setParent :: LinkedTree a -> LinkedTree a -> LinkedTree a
setParent Pusto _ = Pusto
setParent (Branch v l r p) prev = 
    let res = Branch v (setParent l res) (setParent r res) prev
    in res

makeRes :: LinkedTree a -> LinkedTree a -> LinkedTree a -> LinkedTree a
makeRes Pusto Pusto _ = Pusto
makeRes Pusto (Branch v l r p) prev = 
    let res = Branch v (setParent l res) (setParent r res) prev 
    in res
makeRes (Branch v l r p) Pusto prev = 
    let res = Branch v (setParent l res) (setParent r res) prev 
    in res
makeRes (Branch v Pusto r p) lt prev = 
    let res = Branch v (setParent lt res) (setParent r res) prev 
    in res
makeRes (Branch v l r p) lt prev = 
    let res = Branch v (makeRes l lt res) (setParent r res) prev
    in res

remove' :: (Ord a) => LinkedTree a -> a -> LinkedTree a -> LinkedTree a
remove' Pusto _ _ = Pusto
remove' tree @(Branch v l r p) a prev
    | a == v = makeRes r l prev
    | a > v = right
    | otherwise = left
    where right = Branch v (setParent l right) (remove' r a right) prev
          left = Branch v (remove' l a left) (setParent r left) prev

remove :: (Ord a) => LinkedTree a -> a -> LinkedTree a
remove tree a = remove' (goToTheBeginning tree) a Pusto
