module Pseudorandom ( constant, andThen, combine, map, int
                    , float, range, get, Seed
                    , lift, apply, Random) where

{-| This library generates pure (no `Signal` involved) pseudorandom numbers
using the [xorshift algorithm](https://en.wikipedia.org/wiki/Xorshift). The API is much like `Signal`'s.


```elm
import Pseudorandom as R

seed = 1
R.get seed R.Int == 270369
R.get seed (R.map (always <| R.range (100, 110)) [1..10]) ==  [106,105,110,104,102,110,105,106,101,110]
```

# Making random numbers
@docs int, float, range, Seed, get

# Working with random numbers
@docs lift, constant, apply, combine, map, andThen
-}

import List

import Pseudorandom.Internal (..)
import Pseudorandom.Infix (..)

type State s a = s -> (a, s)
type Seed = Int
type Random a = State Seed a

{-| Lifts a function to `Random`. -}
lift : (a -> b) -> Random a -> Random b
lift = (<$>)

{-| Lifts a value into `Random`. -}
constant : a -> Random a
constant = (,) 

{-| Applies a `Random` function to a `Random` value. -}
apply : Random (a -> b) -> Random a -> Random b
apply = (<*>)

{-| Transform a list of `Random` values into a `Random` list of values. Much
like `Signal`'s `combine`. -}
combine : [Random a] -> Random [a]
combine = foldr (\x xs -> (::) <$> x <*> xs) (constant [])

{-| Applies a `Random` function to every element in a list. -}
map : (a -> Random b) -> [a] -> Random [b]
map f = combine << List.map f

{-| Applies a `Random`-producing function to a `Random` value. -}
andThen : (a -> Random b) -> Random a -> Random b
andThen = (=<<)

{-| Produces Int in the range [-2^32, 2^32] (except 0). -}
int : Random Int
int r = let s' = xorshift r in (s', s')

{-| Produces Float in the range [0, 1). -}
float : Random Float
float = first (\n' -> toFloat (abs n' - 1) / -minInt) << int

{-| Produces Int in the specified range. -}
range : (Int, Int) -> Random Int
range rn = first (roundClamp rn) << int

{-| Use a seed to extract a random value. -}
get : Seed -> Random a -> a
get n r = fst <| r n
