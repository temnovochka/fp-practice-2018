module Tests3 where

import Prelude hiding (lookup, foldl, foldr, map, concatMap, 
    filter, maxBy, minBy, sum, product, elem)

import Task3_1
import Task3_2
import Task3_3

a = Zero
b = Succ a
c = Pred a
d = Pred c
e = Succ b

k1 = Pred (Succ (Zero))
k2 = Succ (Pred (Zero))

ra = listToRList [1..10] -- [[], 10, 9, 8, 7, 6, 5, 4, 3, 2, 1]
rb = listToRList [1..9]  -- [[],  9, 8, 7, 6, 5, 4, 3, 2, 1]
rc = listToRList [2..10] -- [[], 10, 9, 8, 7, 6, 5, 4, 3, 2]

l1 = [3,2]
l2 = [3,2,1]

l3 = listToRList [2]
l4 = listToRList [1, 2]

ord_test = [(<=) ra rb == False,
            (<=) ra rc == False,
            (<=) ra ra == True,
            (<=) rb rb == True,
            (<=) rb rc == True,
            (<=) rb ra == True,
            (<=) rc rb == False,
            (<=) rc rc == True,
            (<=) rc ra == True]


square x = x * x
