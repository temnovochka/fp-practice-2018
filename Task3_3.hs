module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

instance Semigroup (PSet a) where
    (<>) a b = mappend a b

-- Объединение множеств
-- Нейтральное значение - пустое множество (чтобы выполнялся осн.закон моноида)
-- Функция добавления - объект входит или в одно множество, или в другое
instance Monoid (PSet a) where
    mempty = PSet{ contains = f } where f _ = False
    mappend PSet{ contains = contains_a } PSet{ contains = contains_b } = PSet{ contains = f }
        where f a | contains_a a == True = True
                  | contains_b a == True = True
                  | otherwise = False

-- Пересечение множеств
-- Нейтральное значение - универсум (чтобы выполнялся осн.закон моноида)
-- Функция добавления - объект входит и в одно множество, и во второе
instance Monoid (PSet a) where
    mempty = PSet{ contains = f } where f _ = True
    mappend PSet{ contains = contains_a } PSet{ contains = contains_b } = PSet{ contains = f }
        where f a | (contains_a a == True) && (contains_b a == True) = True
                  | otherwise = False

-- Разность реализовать не получилось, так как не ясно как определять нейтральное значение
-- Аналогично с декартовым произведением

-- Вспомогательные функции для следующей монады
razn PSet{ contains = contains_a } PSet{ contains = contains_b } = PSet{ contains = f }
    where f a | contains_b a == True = False
              | contains_a a == True = True
              | otherwise = False

objed PSet{ contains = contains_a } PSet{ contains = contains_b } = PSet{ contains = f }
    where f a | contains_a a == True = True
              | contains_b a == True = True
              | otherwise = False

-- Симметричная разность
-- Нейтральное - пустое
-- Берем разности и объединяем :)
instance Monoid (PSet a) where
    mempty = PSet{ contains = f } where f _ = False
    mappend b c = objed (razn b c) (razn c b)

-- С функторами тоже не сложилось. Ведь у нас лежат в мн-ве не значения,
-- а функция, по возвращаемому значению которой можно определять, 
-- принадлежит ли объект мн-ву или нет.
-- И что к кому куда применять?

-- Например, наша функция такая
-- f 1 = True
-- f 2 = True
-- f 3 = True
-- f _ = False

-- А мы хотим ко всем эл-там применить функцию квадрат.
-- Тогда логично, что теперь должно быть мн-во со след.функцией:
-- f 1 = True
-- f 4 = True
-- f 9 = True
-- f _ = False

-- И вообще не факт, что у нас циферки...

-- Чисто в теории должно быть что-то такое:

-- instance Functor (PSet) where
--     fmap f PSet{ contains = fa } = PSet{ contains = fb }
--         where fb (f a) | fa a == True = True
--                        | otherwise = False
