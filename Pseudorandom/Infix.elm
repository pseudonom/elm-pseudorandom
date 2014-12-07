module Pseudorandom.Infix ((<$>), (<*>), (=<<), (<=<)) where

{-|
```elm
seed = 7
get seed (max <$> constant 1 <*> constant 3) == 3
```

# Infix operators
@docs (<$>), (<*>), (=<<), (<=<)
-}

import Pseudorandom.Internal (..)

infixl 4 <$>
{-| The infix operator for lift. -}
(<$>) : (a -> b) -> Random a -> Random b
f <$> r = first f << r

infixl 4 <*>
{-| The infix operator for apply. -}
(<*>) : Random (a -> b) -> Random a -> Random b
rf <*> ra = (\(a, i) -> first ((|>) a) <| rf i) << ra

infixr 1 =<< 
{-| The infix operator for andThen. -}
(=<<) : (a -> Random b) -> Random a -> Random b
f =<< m = uncurry f << m

infixr 1 <=<
{-| Compose `random`-producing functions. -}
(<=<) : (b -> Random c) -> (a -> Random b) -> (a -> Random c)
f <=< g = \x -> f =<< g x 
