module Tests2 where

import Prelude hiding (lookup, foldl, foldr, map, concatMap, 
    filter, maxBy, minBy, sum, product, elem)

import Task2_1
import Task2_2

my_tree = Both 10 10 (Both 9 9 (Both 7 7 Pusto Pusto) Pusto) (Both 11 11 Pusto Pusto)

test_contains = [contains my_tree 9 == True,
                 contains my_tree 12 == False,
                 contains my_tree 8 == False]

test_lookup = [lookup 10 my_tree == 10,
               lookup 11 my_tree == 11,
               lookup 7 my_tree == 7,
               lookup 9 my_tree == 9]

fun1 e lst = e:lst
fun2 lst e = e:lst

test_fold = [foldl fun2 [] [1..10] == Prelude.reverse [1..10],
             foldr fun1 [] [1..10] == [1..10]]


foo :: Integer -> Maybe(Integer, Integer)
foo 0 = Nothing
foo i = Just(i, (i - 1))

t = unfoldr foo 5 == [5, 4, 3, 2, 1]

qwadrat x = x*x
m = map qwadrat [1..10]
