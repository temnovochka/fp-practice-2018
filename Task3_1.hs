module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where
    show Zero = "0"
    show (Succ a) = "Succ (" ++ show a ++ ")"
    show (Pred a) = "Pred (" ++ show a ++ ")"

magicCheck sa pa sb pb f
    | f == 0 = (sa - pa) == (sb - pb)
    | f == 1 = (sa - pa) <= (sb - pb)
    | otherwise = error "404"

count Zero sa pa Zero sb pb f = magicCheck sa pa sb pb f

count a @(Succ c) sa pa b sb pb f = checkB c (sa + 1) pa b sb pb f
count a @(Pred c) sa pa b sb pb f = checkB c sa (pa + 1) b sb pb f
count a @(Zero) sa pa b sb pb f = checkB a sa pa b sb pb f

checkB c sa pa b sb pb f = 
    case b of Succ d -> count c sa pa d (sb + 1) pb f
              Pred d -> count c sa pa d sb (pb + 1) f
              Zero -> count c sa pa b sb pb f

instance Eq WeirdPeanoNumber where
    (==) Zero Zero = True
    (==) (Succ a) (Succ b) = (==) a b
    (==) (Pred a) (Pred b) = (==) a b
    (==) a b = count a 0 0 b 0 0 0

instance Ord WeirdPeanoNumber where
    (<=) Zero Zero = True
    (<=) (Succ a) (Succ b) = (<=) a b
    (<=) (Pred a) (Pred b) = (<=) a b
    (<=) a b = count a 0 0 b 0 0 1

countSP :: WeirdPeanoNumber -> (Integer, Integer) -> (Integer, Integer)
countSP Zero (s, p) = (s, p)
countSP (Succ a) (s, p) = countSP a ((s + 1), p)
countSP (Pred a) (s, p) = countSP a (s, (p + 1))

optimizeSP (s, p)
    | s > p = ((s - p), 1)
    | p > s = ((p - s), -1)
    | s == p = (0, 0)

makeSum :: Integer -> Integer -> WeirdPeanoNumber -> WeirdPeanoNumber
makeSum f _ a
    | f == 1 = Succ a
    | f == -1 = Pred a
    | otherwise = a

countSum :: WeirdPeanoNumber -> Integer -> Integer -> WeirdPeanoNumber
countSum a 0 _ = a
countSum a 1 f = makeSum f 1 a
countSum a sp f = foldr (makeSum f) a [1..sp]

plus :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
plus Zero Zero = Zero
plus Zero a = a
plus a Zero = a
plus a b = countSum Zero sp f 
    where (sp, f) = optimizeSP (countSP a (countSP b (0, 0)))

plusA a f
    | f == 0 = Zero
    | f == 1 = a
    | otherwise = plusA (plus a a) (f - 1)

makeSign a f
    | f == -1 = negate a
    | otherwise = a

umno Zero _ = Zero
umno _ Zero = Zero
umno a b = makeSign (plusA a sp) f
    where (sp, f) = optimizeSP (countSP b (0, 0))

instance Num WeirdPeanoNumber where
    (+) a b = plus a b
    (*) a b = umno a b
    abs a = countSum Zero sp 1 where (sp, _) = optimizeSP (countSP a (0, 0))
    signum a = f where (_, f) = optimizeSP (countSP a (0, 0))
    fromInteger i 
        | i == 0 = Zero
        | i < 0 = countSum Zero (-i) (-1)
        | i > 0 = countSum Zero i 1
    negate a = countSum Zero sp (-f) where (sp, f) = optimizeSP (countSP a (0, 0))

instance Enum WeirdPeanoNumber where
    toEnum i = fromInteger (fromIntegral i)
    fromEnum a = (fromInteger (sp * f)) where (sp, f) = optimizeSP (countSP a (0, 0))

instance Real WeirdPeanoNumber where
    toRational a = toRational (fromEnum a)

makeBeutyResult a b res ost
    | a == b && a < 0 = (res, (-ost))
    | a == b = (res, ost)
    | a < 0 = ((- res), (- ost))
    | otherwise = ((-res), ost)

magic _ Zero _ _ _ = error "are you kidding me?"
magic a b res signa signb
    | absa >= absb = magic (absa - absb) absb (res + 1) signa signb
    | otherwise = makeBeutyResult signa signb res absa
    where (absa, absb) = (abs a, abs b)

instance Integral WeirdPeanoNumber where
    toInteger a = (sp * f) where (sp, f) = optimizeSP (countSP a (0, 0))
    quotRem a b = magic a b Zero (signum a) (signum b)
