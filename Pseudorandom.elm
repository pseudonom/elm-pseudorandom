module Pseudorandom ( pure, (=<<), chain, sequence, mapR, randomInt
                    , randomFloat, randomRange, getRandom, RandomSeed
                    , (<$>), fmap, ap, (<*>), (<=<), Random) where

{-| This library generates pure (no `Signal` involved) pseudorandom numbers
using the [xorshift algorithm](https://en.wikipedia.org/wiki/Xorshift). For
convenience, the PRNG functions expose a monadic API.


```haskell
seed = 1
getRandom seed randomInt == 270369
getRandom seed <| mapR (always <| randomRange (100, 110)) [1..10] ==  [106,105,110,104,102,110,105,106,101,110]
```

# Making random numbers
@docs randomInt, randomFloat, randomRange, RandomSeed, getRandom

# Working with random numbers
@docs fmap, (<$>), pure, ap, (<*>), sequence, mapR, chain, (=<<), (<=<)
-}

import Bitwise

infixr 3 ***
(***) : (a -> b) -> (c -> d) -> (a, c) -> (b, d)
f *** g = (\(x, y) -> (f x, g y))

first : (a -> b) -> (a, c) -> (b, c)
first f = f *** identity

type State s a = s -> (a, s)
type RandomSeed = Int
type Random a = State RandomSeed a

{-| Lifts a function to `Random`. -}
fmap : (a -> b) -> Random a -> Random b
fmap = (<$>)

infixl 4 <$>
{-| The conventional infix operator for fmap -}
(<$>) : (a -> b) -> Random a -> Random b
f <$> r = first f << r

{-| Lifts a value into `Random`. -}
pure : a -> Random a
pure = (,) 

{-| Applies a `Random` function to a `Random` value. -}
ap : Random (a -> b) -> Random a -> Random b
ap = (<*>)

infixl 4 <*>
{-| The conventional infix operator for ap. -}
(<*>) : Random (a -> b) -> Random a -> Random b
rf <*> ra = (\(a, i) -> first ((|>) a) <| rf i) << ra

{-| Transform a list of `Random` values into a `Random` list of values. Much
like `Signal`'s `combine`. -}
sequence : [Random a] -> Random [a]
sequence = foldr (\x xs -> (::) <$> x <*> xs) (pure [])

{-| Applies a `Random` function to every element in a list. -}
mapR : (a -> Random b) -> [a] -> Random [b]
mapR f = sequence << map f

{-| Applies a `Random`-producing function to a `Random` value. -}
chain : (a -> Random b) -> Random a -> Random b
chain = (=<<)

infixr 1 =<< 
{-| The conventional infix operator for chain. -}
(=<<) : (a -> Random b) -> Random a -> Random b
f =<< m = uncurry f << m

infixr 1 <=<
{-| Compose `random`-producing functions. -}
(<=<) : (b -> Random c) -> (a -> Random b) -> (a -> Random c)
f <=< g = \x -> f =<< g x 

-- Semi-arbitrary parameters
a = 13
b = 17
c = 5
bit32 = 4294967295
maxInt = 2147483647
minInt = -2147483648

xorshift : RandomSeed -> RandomSeed
xorshift s = let x = s `Bitwise.xor` (s `Bitwise.shiftLeft` a)
                 y = x `Bitwise.xor` (x `Bitwise.shiftRight` b) in
             y `Bitwise.xor` (y `Bitwise.shiftLeft` c)

{-| Produces Int in the range [-2^32, 2^32] (except 0). -}
randomInt : Random Int
randomInt r = let s' = xorshift r in
              (s', s')

{-| Produces Float in the range [0, 1). -}
randomFloat : Random Float
randomFloat = first (\n' -> toFloat (abs n' - 1) / -minInt) << randomInt

{-| Produces Int in the specified range. -}
randomRange : (Int, Int) -> Random Int
randomRange rn = first (roundClamp rn) << randomInt

{-| Use a seed to extract a random value. -}
getRandom : RandomSeed -> Random a -> a
getRandom n r = fst <| r n

roundClamp : (number, number) -> number -> number
roundClamp (l, g) i = l + (i - l) % (g - l + 1)
