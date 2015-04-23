module Pseudorandom.Internal (first, Random, Seed, xorshift, maxInt, minInt) where

import Bitwise

first : (a -> b) -> (a, c) -> (b, c)
first f (a, b) = (f a, b)

type State s a = s -> (a, s)
type Seed = Int
type Random a = State Seed a

maxInt = 2147483647
minInt = -2147483648
-- Semi-arbitrary parameters
a = 13
b = 17
c = 5

xorshift : Seed -> Seed
xorshift s = let x = s `Bitwise.xor` (s `Bitwise.shiftLeft` a)
                 y = x `Bitwise.xor` (x `Bitwise.shiftRight` b) in
             y `Bitwise.xor` (y `Bitwise.shiftLeft` c)
