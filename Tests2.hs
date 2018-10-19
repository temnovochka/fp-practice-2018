module Tests2 where

import Prelude hiding (lookup, foldl, foldr, map, concatMap, 
    filter, maxBy, minBy, sum, product, elem)

import Task2_1
import Task2_2

vershina Pusto = error "404 not found vershina"
vershina (Both k _ _ _) = k

my_tree = Both 10 10 (Both 8 8 (Both 5 5 (Both 1 1 Pusto (Both 3 3 (Both 2 2 Pusto Pusto) (Both 4 4 Pusto Pusto))) (Both 6 6 Pusto (Both 7 7 Pusto Pusto))) (Both 9 9 Pusto Pusto)) (Both 11 11 Pusto Pusto)

my_tree_2 = Both 8 8 (Both 5 5 (Both 1 1 Pusto (Both 3 3 (Both 2 2 Pusto Pusto) (Both 4 4 Pusto Pusto))) (Both 6 6 Pusto (Both 7 7 Pusto Pusto))) (Both 9 9 Pusto Pusto)

my_tree_3 = Both 1 1 Pusto (Both 3 3 (Both 2 2 Pusto Pusto) (Both 4 4 Pusto Pusto))

my_tree_4 = Both 5 5 (Both 1 1 Pusto (Both 3 3 (Both 2 2 Pusto Pusto) (Both 4 4 Pusto Pusto))) (Both 6 6 Pusto (Both 7 7 Pusto Pusto))

my_tree_5 = Both 3 3 (Both 2 2 Pusto Pusto) (Both 4 4 Pusto Pusto)


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

ver = vershina my_tree
ver_2 = vershina my_tree_2
ver_3 = vershina my_tree_3
ver_4 = vershina my_tree_4
ver_5 = vershina my_tree_5

cifra = magicCiferka ver my_tree 0
cifra_2 = magicCiferka ver_2 my_tree_2 0

check_kmean = [kMean 0 my_tree == kMeanCheck 0 my_tree,
               kMean 0 my_tree_2 == kMeanCheck 0 my_tree_2,
               kMean 0 my_tree_3 == kMeanCheck 0 my_tree_3,
               kMean 0 my_tree_4 == kMeanCheck 0 my_tree_4,
               kMean 0 my_tree_5 == kMeanCheck 0 my_tree_5,
               kMean 1 my_tree == kMeanCheck 1 my_tree,
               kMean 1 my_tree_2 == kMeanCheck 1 my_tree_2,
               kMean 1 my_tree_3 == kMeanCheck 1 my_tree_3,
               kMean 1 my_tree_4 == kMeanCheck 1 my_tree_4,
               kMean 1 my_tree_5 == kMeanCheck 1 my_tree_5,
               kMean 2 my_tree == kMeanCheck 2 my_tree,
               kMean 2 my_tree_2 == kMeanCheck 2 my_tree_2,
               kMean 2 my_tree_3 == kMeanCheck 2 my_tree_3,
               kMean 2 my_tree_4 == kMeanCheck 2 my_tree_4,
               kMean 2 my_tree_5 == kMeanCheck 2 my_tree_5,
               kMean 3 my_tree == kMeanCheck 3 my_tree,
               kMean 3 my_tree_2 == kMeanCheck 3 my_tree_2,
               kMean 3 my_tree_3 == kMeanCheck 3 my_tree_3,
               kMean 3 my_tree_4 == kMeanCheck 3 my_tree_4,
               kMean 4 my_tree == kMeanCheck 4 my_tree,
               kMean 4 my_tree_2 == kMeanCheck 4 my_tree_2,
               kMean 4 my_tree_4 == kMeanCheck 4 my_tree_4,
               kMean 5 my_tree == kMeanCheck 5 my_tree,
               kMean 5 my_tree_2 == kMeanCheck 5 my_tree_2,
               kMean 5 my_tree_4 == kMeanCheck 5 my_tree_4,
               kMean 6 my_tree == kMeanCheck 6 my_tree,
               kMean 6 my_tree_2 == kMeanCheck 6 my_tree_2,
               kMean 6 my_tree_4 == kMeanCheck 6 my_tree_4,
               kMean 7 my_tree == kMeanCheck 7 my_tree,
               kMean 7 my_tree_2 == kMeanCheck 7 my_tree_2,
               kMean 8 my_tree == kMeanCheck 8 my_tree,
               kMean 9 my_tree == kMeanCheck 9 my_tree,
               kMean 10 my_tree == kMeanCheck 10 my_tree]


magic_ciferka = [magicCiferka ver my_tree 0,
                 magicCiferka ver_2 my_tree_2 0,
                 magicCiferka ver_3 my_tree_3 0,
                 magicCiferka ver_4 my_tree_4 0,
                 magicCiferka ver_5 my_tree_5 1]


matrix1 = [[1,2,3],[4,5,6],[7,8,9]]
matrix2 = [[1,2,3,10],[4,5,6,11],[7,8,9,12]]
matrix3 = [[1,2,3],[4,5,6],[7,8,9],[10,11,12]]

diagonal_test = [diagonal matrix1,
                 diagonal matrix2,
                 diagonal matrix3]
