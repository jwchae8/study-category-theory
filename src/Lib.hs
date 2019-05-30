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
cop2 :: Int Bool -> (Int, Bool)
cop2 x y = if y then (x+1, 0) else (x,0)

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
