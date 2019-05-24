module Lib
    ( myId,
      compose,
      fid, idf
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
