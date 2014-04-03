module Pseudorandom ( pure, (>>=), chain, sequence, mapM, randomInts
                    , randomFloats, randomRange) where

{-| This library generates pure (no `Signal` involved) pseudorandom numbers
using the xorshift algorithm (https://en.wikipedia.org/wiki/Xorshift). For
convenience, the PRNG functions expose a monadic API.

`Random a` is a type synonym for `Int -> (a, Int)`.

```haskell
seed = 7
randomInts 2 seed == ([-1459243860,11355432],-1459243860)
randomInts 1 -1459243860 == ([-308848668],-308848668)
randomInts 3 seed == ([-308848668,-1459243860,11355432],-308848668)

single : Rand Int
single = (\(r, s) -> (head r, s)) . randomInts 1
```

# Making random numbers
@docs randomInts, randomFloats, randomRange

# Working with random numbers
@docs pure, chain, (>>=), sequence, mapM
-}

import Bitwise (xor, shiftLeft, shiftRight)

infixr 3 ***
(***) : (a -> b) -> (c -> d) -> (a, c) -> (b, d)
f *** g = (\(x, y) -> (f x, g y))

type State s a = s -> (a, s)
type RandomSeed = Int
type Random a = State RandomSeed a

{-| Lifts a value into the `Random` monad. -}
pure : a -> Random a
pure a = (,) a

{-| Applies a `Random` function to a `Random` value. -}
chain : Random a -> (a -> Random b) -> Random b
chain = (>>=)

{-| The conventional infix operator for chain. -}
infixl 1 >>=
(>>=) : Random a -> (a -> Random b) -> Random b
m >>= f = uncurry f . m

{-| Transform a list of `Random` values into a `Random` list of values. Much
like `Signal`'s `combine`. -}
sequence : [Random a] -> Random [a]
sequence ms = foldr (\m m' -> m >>= \x -> m' >>= pure . (::) x) (pure []) ms

{-| Applies `Random` function to every element in a list. -}
mapM : (a -> Random b) -> [a] -> Random [b]
mapM f = sequence . map f

-- Semi-arbitrary parameters
a = 13
b = 17
c = 5
bit32 = 4294967295
maxInt = 2147483647
minInt = -2147483648

xorshift : RandomSeed -> RandomSeed
xorshift s = let x = s `xor` (s `shiftLeft` a)
                 y = x `xor` (x `shiftRight` b) in
             y `xor` (y `shiftLeft` c)

{-| Produces several Ints in the range [-2^32, 2^32] (except 0). -}
randomInts : Int -> Random [Int]
randomInts n r = repeat n (\(xs, s) ->
                            let s' = xorshift s in
                            (s' :: xs, s')) ([], r)

{-| Produces several Floats in the range [0, 1). -}
randomFloats : Int -> Random [Float]
randomFloats n = (map (\n' -> toFloat (abs n' - 1) / -minInt) *** id) . randomInts n

{-| Produces several Ints in the specified range. -}
randomRange : (Int, Int) -> Int -> Random [Int]
randomRange rn n = (map (roundClamp rn) *** id) . randomInts n

roundClamp : (number, number) -> number -> number
roundClamp (l, g) i = l + (i - l) `mod` (g - l + 1)

repeat : Int -> (a -> a) -> a -> a
repeat n f a =
  case n of
    0 -> a
    _ -> repeat (n - 1) f (f a)
