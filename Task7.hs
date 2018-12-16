module Task7 where

import Todo(todo)

data Deque a = Deque [a] Int [a] Int deriving Show

-- Пустая очередь
empty :: Deque a
empty = Deque [] 0 [] 0

qBalance :: Deque a -> Deque a
qBalance q @(Deque [] 0 [] 0) = q
qBalance q @(Deque l lenl r lenr)
    | lenl == 0 && lenr == 1 = q
    | lenl >= 2 * lenr = Deque l1 len1 (r ++ reverse l2) len2
    | lenr >= 2 * lenl = Deque (l ++ reverse r2) len2 r1 len1
    | otherwise = q
    where (l1, l2) = splitAt halflen l
          (r1, r2) = splitAt halflen r
          halflen = (lenl + lenr) `div` 2
          len1 = halflen
          len2 = lenr + lenl - halflen

-- Добавление в начало очереди (соответствует enqueue из лекции)
pushFront :: Deque a -> a -> Deque a
pushFront (Deque [] 0 [] 0) el = Deque [] 0 [el] 1
pushFront (Deque l lenl r lenr) el = qBalance $ Deque (el:l) (lenl + 1) r lenr

-- Удаление из начала очереди
popFront :: Deque a -> (a, Deque a)
popFront (Deque [] 0 [] 0) = error "404 empty"
popFront (Deque [] 0 [el] 1) = (el, empty)
popFront (Deque [el] 1 r lenr) = (el, qBalance $ Deque [] 0 r lenr)
popFront (Deque (h:t) lenl r lenr) = (h, qBalance $ Deque t (lenl - 1) r lenr)

-- Добавление в конец очереди
pushBack :: Deque a -> a -> Deque a
pushBack (Deque [] 0 [] 0) el = Deque [] 0 [el] 1
pushBack (Deque l lenl r lenr) el = qBalance $ Deque l lenl (el:r) (lenr + 1)

-- Удаление из конца очереди (соответствует dequeue из лекции)
popBack :: Deque a -> (a, Deque a)
popBack (Deque [] 0 [] 0) = error "404 empty"
popBack (Deque [el] 1 [] 0) = (el, empty)
popBack (Deque l lenl [el] 1) = (el, qBalance $ Deque l lenl [] 0)
popBack (Deque l lenl (h:t) lenr) = (h, qBalance $ Deque l lenl t (lenr - 1))
