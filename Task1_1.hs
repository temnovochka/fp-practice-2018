module Task1_1 where

import Todo(todo)

data Operation = Plus | Minus | Umnoshit
                 deriving(Show, Eq)

data Term = IntConstant{ intValue :: Int }           -- числовая константа
            | Variable{ varName :: String }          -- переменная
            | BinaryTerm{ lhv :: Term, rhv :: Term, oper :: Operation } -- бинарная операция
            deriving(Show, Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) l r = BinaryTerm l r Plus
(|-|) :: Term -> Term -> Term
(|-|) l r = BinaryTerm l r Minus
(|*|) :: Term -> Term -> Term
(|*|) l r = BinaryTerm l r Umnoshit

infixl 1 |+|
infixl 1 |-|
infixl 2 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement (Variable v) | v == varName = replacement
                                            | otherwise = Variable v
replaceVar varName replacement (BinaryTerm l r o) = 
    let mReplaceVar = replaceVar varName replacement in
    BinaryTerm (mReplaceVar l) (mReplaceVar r) o
replaceVar varName replacement expression = expression

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
makeBTBetter (IntConstant l) (IntConstant r) o = case o of Plus -> IntConstant (l + r)
                                                           Minus -> IntConstant (l - r)
                                                           Umnoshit -> IntConstant (l * r)
makeBTBetter le @(IntConstant l) r o | l == 0 = case o of Plus -> r
                                                          Minus -> BinaryTerm le r o
                                                          Umnoshit -> IntConstant 0
                                     | l == 1 = case o of Umnoshit -> r
                                                          otherwise -> BinaryTerm le r o
                                     | otherwise = BinaryTerm le r o
makeBTBetter l ri @(IntConstant r) o | r == 0 = case o of Plus -> l
                                                          Minus -> l
                                                          Umnoshit -> IntConstant 0
                                     | r == 1 = case o of Umnoshit -> l
                                                          otherwise -> BinaryTerm l ri o
                                     | otherwise = BinaryTerm l ri o
makeBTBetter l r o = BinaryTerm l r o

evaluate (BinaryTerm l r o) = makeBTBetter (evaluate l) (evaluate r) o
evaluate expression = expression
