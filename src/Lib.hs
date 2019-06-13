module Lib
    ( myId,
      compose,
      fid, idf,
      mymemo,
      boolOne, boolTwo, boolThree,
      boolFour, boolFive, boolSix,
      boolSeven, boolEight, boolNine,
      safe_root_reciprocal,
      em, me,
      add2Double, double2Add
    ) where


-- ch01 challenge

-- 1.1 Identity function
myId x = x

-- 1.2 Composing function
compose f g = f . g

-- 1.3 Testing composition respects identity
fid f = compose f myId
idf f = compose myId f

-- 1.4 Yes. if composability guaranteed

-- 1.5 No. Assume A and B are friends, B and C are friends. It does not imply A and C are friends

-- 1.6 self cycle(existence of identity) + composability(a->b->c implies a->c)

-- ch02 challenge

-- 2.1 Memoizing function

mymemo f = (map f [0 ..] !!)

-- 2.2 No it would produce the same result always

-- 2.3 I don't know random generator via seed in haskell sry

-- 2.4 pure : a / dirty : b, c, d

-- 2.5

boolOne :: Bool -> Bool
boolOne x = x

boolTwo :: Bool -> Bool
boolTwo x = not x

boolThree :: Bool -> Bool
boolThree x = True

boolFour :: Bool -> Bool
boolFour x = False

boolFive :: Bool -> Bool
boolFive x = if x then x else error "Do not give me False"

boolSix :: Bool -> Bool
boolSix x = if not x then x else error "Do not give me True"

boolSeven :: Bool -> Bool
boolSeven x = if x then not x else error "Do not give me False"

boolEight :: Bool -> Bool
boolEight x = if not x then not x else error "Do not give me True"

boolNine :: Bool -> Bool
boolNine x = error "Do not call me"

-- 2.6 bool -> bool (4 cases) void -> bool | unit | void, bool -> unit, unit -> unit | bool(false / true)

-- 3.1 a) node -- identity --> node
-- 3.1 b) node -- identity --> node // node -- (edge repeated n times) --> node
-- 3.1 c) node1 -- identity --> node1 // node2 -- identity --> node2 // node1 -- edge --> node2
-- 3.1 d) node -- identity --> node // node -- (alphabet sequence) --> node
-- 3.2 a) partial order b) partial order
-- 3.3 a (&& or ||) (b (&& or ||) c) == (a (&& or ||) b) (&& or ||) c // a && true == a | true && a == a // a && false == a | false && a == a
-- 3.4 node -- identity(and true) --> node // node -- and false --> node
-- 3.5 node -- identity(add 0) --> node // node -- add 1 and mod 3 --> node // node -- add 2 and mod 3 --> node

-- 4.1
type SafeValue a = (a, Bool)
--safeId :: A -> SafeValue A
safeId x = (x, True)


safeCompose :: (a -> SafeValue b) -> (b -> SafeValue c) -> (a -> SafeValue c)
safeCompose f g = \x ->
  let (value1, valid1) = f x
      (value2, valid2) = g value1
    in (value2, valid1 && valid2)

-- 4.2
safe_reprocical :: Int -> SafeValue Float
safe_reprocical x = if x /= 0 then safeId ( 1 / (fromIntegral x) ) else (0, False)

-- 4.3
safe_root :: Float -> SafeValue Float
safe_root x = if x >= 0 then safeId (sqrt x) else (0, False)
safe_root_reciprocal = safeCompose safe_reprocical safe_root

-- 5.1
-- Use initial object uniqueness proof in opposite way

-- 5.2
-- c >= a && c >= b && c' >= c && c' >= a && c' >= b

-- 5.3
-- c <= a && c <= b && c' <= c && c' <= a && c' <= b

-- 5.4
-- I use Haskell so pass this problem

-- 5.5
cop :: Either Int Bool -> Int
cop x = case x of
  (Left v) -> v
  (Right v) -> if v then 1 else 0

-- 5.6 0 cannot decided to be Left 0 or Right False, 1 cannot decided to be Left 1 or Right True

-- 5.7 ambiguousness problem solved, but Int -> Either Int Bool is limited to maxint-2 so Either is still better

-- 5.8 
cop2 :: Int -> Bool -> (Int, Bool)
cop2 x y = if y then (x+1, False) else (x, False)

-- 6.1
me :: Maybe a -> Either () a
me x = case x of 
  (Just v) -> Right v
  Nothing -> Left ()

em :: Either () a -> Maybe a
em x = case x of
  (Left ()) -> Nothing
  (Right v) -> Just v

-- 6.2~6.3: pass

-- 6.4: haskell - add Square on data Shape, and define circ / area for Square. C++/Java, create total new class Square

-- 6.5
add2Double :: Either a a -> (Bool, a)
add2Double x = case x of
  (Left v) -> (True, v)
  (Right v) -> (False, v)

double2Add :: (Bool, a) -> Either a a
double2Add x = case x of
  (True, v) -> Left v
  (False, v) -> Right v


-- 7.1 it does not obey preservation of identity in case of Just value. So fmap _ _ = Nothing cannot be functor of Maybe

-- 7.2
-- preservation of identity
-- fmap id g
-- = id . g
-- = g
-- = id g
-- preservation of composition
-- fmap (g . f) h 
-- g . f . h
-- g . (fmap f h)
-- fmap g (fmap f h)
-- (fmap g . fmap f) h

-- 7.3: pass

-- 7.4
-- preservation of identity
-- fmap id Nil
-- = Nil
-- = id Nil
-- fmap id (Cons x t)
-- = Cons (id x) (fmap id t)
-- = Cons (id x) t
-- = Cons x t
-- = id (Cons x t)
-- preservation of composition
-- fmap (g . f) Nil
-- = Nil
-- = fmap g Nil
-- = fmap g (fmap f Nil)
-- = (fmap g . fmap f) Nil
-- fmap (g . f) (Cons x t)
-- = Cons ((g . f) x)) (fmap (g . f) t)
-- = Cons ((g . f) x)) (fmap g (fmap f t))
-- = Cons ((g (f x)) (fmap g (fmap f t))
-- = fmap g (Cons (f x) (fmap f t))
-- = fmap g (fmap f (Cons x t))
-- = (fmap g . fmap f) (Cons x t)

-- 8.1 
-- instance Functor (Pair a) where
--   fmap f (Pair a b) = Pair a (f b)
-- preservation of identity
-- fmap id (Pair a b)
-- = Pair a (id b)
-- = Pair a b
-- = id (Pair a b)
-- preservation of composition
-- fmap (g . f) (Pair a b)
-- = Pair a ((g . f) b)
-- = Pair a (g (f b))
-- = fmap g (Pair a (f b))
-- = fmap g (fmap f (Pair a b))
-- = (fmap g . fmap f) (Pair a b)
-- Do the same thing for (Pair b) then we can show it's bifunctor

--  class Bifunctor f where
--    bimap :: (a -> c) -> (b -> d) -> f a b -> f c d
--    bimap g h = first g . second h
--    first :: (a -> c) -> f a b -> f c b
--    first g = bimap g id
--    second :: (b -> d) -> f a b -> f a d
--    second = bimap id

-- bimap
-- bimap f g (Pair a b)
-- = (first f . second g) (Pair a b)
-- = first f (second g (Pair a b))
-- = first f (Pair a (g b))
-- = Pair (f a) (g b)
-- first
-- first f (Pair a b)
-- = (first f . second id) (Pair a b)
-- = Pair (f a) b
-- second f (Pair a b)
-- = (first id . second f) (Pair a b)
-- = Pair a (f b)

-- 8.2
-- maybeToMaybe' :: Maybe a -> Either (Const () a) (Identity a)
-- maybeToMaybe' Nothing = Left $ Const ()
-- maybeToMaybe' (Just v) = Right $ Identity v
-- maybe'ToMaybe :: Either (Const () a) (Identity a)
-- maybe'ToMaybe (Left (Const ())) = Nothing
-- maybe'ToMaybe (Right (Identity v)) = Just v
-- maybeToMaybe' . maybe'ToMaybe = id
-- maybeToMaybe' . maybe'ToMaybe (Left (Const())
-- = maybeToMaybe' Nothing
-- = (Left (Const ())
-- = id (Left (Const ())
-- maybeToMaybe' . maybe'ToMaybe (Right (Identity v))
-- = maybeToMaybe' (Just v)
-- = (Right (Identity v))
-- = id (Right (Identity v))
-- maybe'ToMaybe . maybeToMaybe = id
-- maybe'ToMaybe . maybeToMaybe' Nothing
-- = maybe'ToMaybe (Left (Const ()))
-- = Nothing
-- = id Nothing
-- maybe'ToMaybe . maybeToMaybe (Just v)
-- = maybe'ToMaybe (Right (Identity v))
-- = Just v
-- = id (Just v)
-- 8.3
-- instance (Functor b) => Functor (PreList a) where
--   fmap f Nil = Nil
--   fmap f (Cons a b) = Cons a (fmap f b)
-- show id & composition for this case, and fixing `a` case
-- 8.4
-- instance Bifunctor (K2 c) where
--   bimap _ _ (K2 c) = K2 c
-- instance Bifunctor (Fst a) where
--   bimap f _ (Fst a) = Fst (f a)
-- instance Bifunctor (Snd b) where
--   bimap _ g (Snd b) = Snd (g b)
-- 8.5 : pass
-- 8.6 It's a profunctor
-- stdmapget :: Key -> maybe T
-- instance Profunctor stdmapget where
--   dimap f g stdmapget = lmap f . rmap g
--   lmap f stdmapget = \key -> stdmapget (f \key)
--   rmap g stdmapget = \key -> fmap (g (stdmapget \key))
