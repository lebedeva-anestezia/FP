{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Primitive where

import Prelude (Show,Read,error)

---------------------------------------------
-- Синтаксис лямбда-выражений

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x %+ y
example2' x   = \y -> x %+ y
example2''    = \x -> \y -> x %+ y
example2'''   = \x y -> x %+ y
example2''''  = let z = \x y -> x %+ y in z
example2''''' = z where
    z x = \y -> x %+ y

-- Зацикленное выражение
undefined = undefined

-- Ниже следует реализовать все термы, состоящие из undefined заглушки.
-- Любые термы можно переписывать (natEq и natLt --- хорошие кандидаты).

-------------------------------------------
-- Примитивные типы

-- Тип с единственным элементом
data Unit = Unit deriving (Show,Read)

-- Пара, произведение
data Pair a b = Pair { fst :: a, snd :: b } deriving (Show,Read)

-- Вариант, копроизведение
data Either a b = Left a | Right b deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit a
data Maybe a = Nothing | Just a deriving (Show,Read)

-- Частый частный случай, изоморфно Either Unit Unit
data Bool = False | True deriving (Show,Read)

-- Следует отметить, что встроенный if с этим Bool использовать нельзя,
-- зато case всегда работает.

-- Ну или можно реализовать свой if
if' True a b = a
if' False a b = b

-- Трихотомия. Замечательный тип, показывающий результат сравнения
data Tri = LT | EQ | GT deriving (Show,Read)

-------------------------------------------
-- Булевы значения

-- Логическое "НЕ"
not :: Bool -> Bool
not True = False
not False = True

infixr 3 &&
-- Логическое "И"
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && _ = False

infixr 2 ||
-- Логическое "ИЛИ"
(||) :: Bool -> Bool -> Bool
True  || _ = True
False || x = x

-------------------------------------------
-- Натуральные числа

data Nat = Zero | Succ Nat deriving (Show,Read)

natZero = Zero     -- 0
natOne = Succ Zero -- 1

-- Сравнивает два натуральных числа
natCmp :: Nat -> Nat -> Tri
natCmp n m = if' (natLt n m) (LT) (if' (natEq n m) (EQ) (GT))

-- n совпадает с m 
natEq :: Nat -> Nat -> Bool
natEq Zero     Zero     = True
natEq Zero     (Succ _) = False
natEq (Succ _) Zero     = False
natEq (Succ n) (Succ m) = natEq n m

-- n меньше m
natLt :: Nat -> Nat -> Bool
natLt Zero     Zero     = False
natLt Zero     (Succ m) = True
natLt (Succ n) Zero     = False
natLt (Succ n) (Succ m) = natLt n m

infixl 6 +.
-- Сложение для натуральных чисел
(+.) :: Nat -> Nat -> Nat
Zero     +. m = m
(Succ n) +. m = Succ (n +. m)

infixl 6 -.
-- Вычитание для натуральных чисел
(-.) :: Nat -> Nat -> Nat
Zero -. n = Zero               
(Succ n) -. Zero = Succ n
(Succ n) -. (Succ m) = n -. m

infixl 7 *.
-- Умножение для натуральных чисел
(*.) :: Nat -> Nat -> Nat
Zero     *. m = Zero
(Succ n) *. m = m +. (n *. m)

-- Целое и остаток от деления n на m
natDivMod :: Nat -> Nat -> Pair Nat Nat
natDivMod n Zero = error "division by zero"
natDivMod n m = if' (natLt n m) (Pair Zero n) (Pair (Succ $ natDiv (n -. m) m) (natMod (n -. m) m))

natDiv n = fst . natDivMod n -- Целое
natMod n = snd . natDivMod n -- Остаток


-- Поиск GCD алгоритмом Евклида (должен занимать 2 (вычислителельная часть) + 1 (тип) строчки)
gcd :: Nat -> Nat -> Nat
gcd Zero Zero = error "gcd for zero zero"
gcd n Zero = n
gcd n (Succ m) = gcd (Succ m) (natMod n (Succ m))

-------------------------------------------
-- Целые числа

-- Требуется, чтобы представление каждого числа было единственным
data Int = Pos Nat | Neg Nat deriving (Show,Read)

intZero   = Pos Zero   -- 0
intOne    = Pos $ Succ Zero     -- 1
intNegOne = Neg Zero -- -1

-- n -> - n
intNeg :: Int -> Int
intNeg (Pos Zero) = Pos Zero
intNeg (Pos (Succ n)) = Neg n
intNeg (Neg n) = Pos $ Succ n

-- Дальше также как для натуральных
intCmp :: Int -> Int -> Tri
intCmp n m = if' (intLt n m) (LT) (if' (intEq n m ) (EQ) (GT))

intEq :: Int -> Int -> Bool
intEq (Pos n) (Neg m) = False
intEq (Neg n) (Pos m) = False
intEq (Pos n) (Pos m) = natEq n m
intEq (Neg n) (Neg m) = natEq n m

intLt :: Int -> Int -> Bool
intLt (Neg n) (Pos m) = True
intLt (Pos n) (Neg m) = False
intLt (Pos n) (Pos m) = natLt n m
intLt (Neg n) (Neg m) = natLt m n

infixl 6 .+., .-.
-- У меня это единственный страшный терм во всём файле
(.+.) :: Int -> Int -> Int
(Pos Zero) .+. n = n
(Pos (Succ n)) .+. (Neg Zero) = Pos n
(Pos n) .+. (Pos m) = Pos $ n +. m
(Neg n) .+. (Neg m) = Neg $ Succ n +. m
(Pos (Succ n)) .+. (Neg (Succ m)) = Pos n .+. Neg m
(Neg n) .+. (Pos m) = Pos m .+. Neg n
       
(.-.) :: Int -> Int -> Int
n .-. m = n .+. (intNeg m)

infixl 7 .*.
(.*.) :: Int -> Int -> Int
(Pos n) .*. (Pos m) = Pos $ n *. m
(Pos n) .*. (Neg m) = intNeg . Pos $ n *. (Succ m)
(Neg n) .*. (Neg m) = Pos $ (Succ n) *. (Succ m)
(Neg n) .*. (Pos m) = (Pos m) .*. (Neg n)

-- абсолютное значение
abs :: Int -> Nat
abs (Pos n) = n
abs (Neg n) = Succ n

-- целочисленное деление
intDiv :: Int -> Int -> Int
intDiv n (Pos Zero) = error "division by zero"
intDiv (Pos n) (Pos (Succ m)) = Pos $ natDiv n (Succ m)
intDiv (Neg n) (Pos (Succ m)) = intNeg (Pos $ natDiv (Succ n) (Succ m))
intDiv (Pos n) (Neg m) = intDiv (intNeg $ Pos n) (Pos $ Succ m)
intDiv (Neg n) (Neg m) = intDiv (Pos $ Succ n) (Pos $ Succ m)

-------------------------------------------
-- Рациональные числа

data Rat = Rat Int Nat

ratNeg :: Rat -> Rat
ratNeg (Rat x y) = Rat (intNeg x) y

-- У рациональных ещё есть обратные элементы
ratInv :: Rat -> Rat
ratInv (Rat (Pos Zero) _) = error "No inverse element for zero"
ratInv (Rat (Pos n) m) = Rat (Pos m) n 
ratInv (Rat (Neg n) m) = Rat (intNeg (Pos m)) (Succ n)

-- Дальше как обычно
ratCmp :: Rat -> Rat -> Tri
ratCmp (Rat n m) (Rat a b) = intCmp (n .*. (Pos b)) (a .*. (Pos m))

ratEq :: Rat -> Rat -> Bool
ratEq (Rat n m) (Rat a b) = intEq (n .*. (Pos b)) (a .*. (Pos m))

ratLt :: Rat -> Rat -> Bool
ratLt (Rat n m) (Rat a b) = intLt (n .*. (Pos b)) (a .*. (Pos m))

infixl 7 %+, %-
(%+) :: Rat -> Rat -> Rat
(Rat n m) %+ (Rat a b) = simpleFrac $ Rat ((n .*. (Pos b)) .+. (a .*. (Pos m))) (m *. b)

(%-) :: Rat -> Rat -> Rat
n %- m = n %+ (ratNeg m)

infixl 7 %*, %/
(%*) :: Rat -> Rat -> Rat
(Rat n m) %* (Rat a b) = simpleFrac $ Rat (n .*. a) (m *. b)

(%/) :: Rat -> Rat -> Rat
n %/ m = n %* (ratInv m)

-- Сокращение дроби
simpleFrac :: Rat -> Rat
simpleFrac (Rat n m) = Rat (intDiv n (Pos g)) (natDiv m g) 
			where g = gcd (abs n) m

-------------------------------------------
-- Операции над функциями.
-- Определены здесь, но использовать можно и выше

infixr 9 .
f . g = \ x -> f (g x)

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   a b c = gcd a (gcd b c)
example3'  a b c = gcd a $ gcd b c
example3'' a b c = ($) (gcd a) (gcd b c)

-- И ещё эквивалентные определения
example4  a b x = (gcd a (gcd b x))
example4' a b = gcd a . gcd b
