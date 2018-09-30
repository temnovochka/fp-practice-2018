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


sinorm = [normalize 0 (-pi/2) == 0,
          normalize pi (-pi/2) == 0,
          normalize (3*pi/2) (-pi/2) == -pi/2,
          normalize (pi/2) (-pi/2) == pi/2,
          normalize (2*pi) (-pi/2) == 0,
          normalize (3*pi/4) (-pi/2) == pi/4,
          normalize (5*pi/4) (-pi/2) == -pi/4]

conorm = [normalize 0 0 == 0,
          normalize pi 0 == pi,
          normalize (3*pi/2) 0 == pi/2,
          normalize (pi/2) 0 == pi/2,
          normalize (2*pi) 0 == 0,
          normalize (3*pi/4) 0 == 3*pi/4,
          normalize (5*pi/4) 0 == 3*pi/4]

testTeylorSin x = abs (Task1_2.sin x - Prelude.sin x) <= 1e-14
testTeylorCos x = abs (Task1_2.cos x - Prelude.cos x) <= 1e-14

sinTest = [testTeylorSin 0,
           testTeylorSin pi,
           testTeylorSin (pi/2),
           testTeylorSin (100*pi),
           testTeylorSin (3*pi/4),
           testTeylorSin (5*pi/4),
           testTeylorSin (pi/6)]

cosTest = [testTeylorCos 0,
           testTeylorCos pi,
           testTeylorCos (pi/2),
           testTeylorCos (100*pi),
           testTeylorCos (3*pi/4),
           testTeylorCos (5*pi/4),
           testTeylorCos (pi/6)]
