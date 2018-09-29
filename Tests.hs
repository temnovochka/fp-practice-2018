module Tests where

import Task1_2
import Task1_1

test_doesSquareBetweenExist = 
 [doesSquareBetweenExist 2 3 == False, -- нет
  doesSquareBetweenExist 3 5 == True, -- между числами 4
  doesSquareBetweenExist 5 7 == False, -- нет
  doesSquareBetweenExist 5 9 == False, -- нет (но верхнее число квадрат)
  doesSquareBetweenExist 5 10 == True, -- есть 9
  doesSquareBetweenExist 4 7 == True] -- нижняя граница 4

test_isPrime = [isPrime 1 == False,
                isPrime 2 == True,
                isPrime 3 == True,
                isPrime 4 == False,
                isPrime 5 == True,
                isPrime 6 == False,
                isPrime 7 == True,
                isPrime 8 == False,
                isPrime 9 == False,
                isPrime 10 == False,
                isPrime 11 == True,
                isPrime 12 == False]

expr = IntConstant 4 |+| Variable "A" |*| IntConstant 5 |-| IntConstant 3 |+| Variable "V" |+| IntConstant 10 |-| IntConstant 1 |*| Variable "A"
replace_var_1 = replaceVar "A" (Variable "N") expr
evaluate_1 = evaluate expr

tmp = replaceVar "A" (IntConstant 5) expr
replace_var_2 = replaceVar "V" (IntConstant 5) tmp
evaluate_2 = evaluate replace_var_2


myAwesomePoints = [(2, 4), (3, -8), (1, 2), (2, 4)]
squareOfMyAwesomeFigureMadeWithAwesomePoints = shapeArea myAwesomePoints
