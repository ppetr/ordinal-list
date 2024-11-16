-- Copyright 2024 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
-- License for the specific language governing permissions and limitations
-- under the License.
{-# LANGUAGE DeriveFunctor, ScopedTypeVariables #-}
{-# OPTIONS_GHC -W #-}
module Data.Ordinal.ListExp
    ( OList1(..)
    , withPrefix
    , omega
    ) where

import           Data.Functor                   ( (<&>) )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as E
import           Data.Semigroup
import           Data.Stream                    ( Stream(Cons) )
import qualified Data.Stream                   as S

-- | Non-empty ordinal-indexed list up to ω^ω.
--
-- TODO: Use a strict-spine `NonEmpty` list if possible in `End` to enforce
-- that it's never infinite.
data OList1 a = End (NonEmpty a) | Power !(OList1 (Stream a)) [a]
  deriving (Functor)

-- | Prepends a given prefix to an ordinal.
withPrefix :: [a] -> OList1 a -> OList1 a
withPrefix []       y            = y
withPrefix (x : xl) (End y     ) = End ((x :| xl) <> y)
withPrefix x        (Power ys y) = Power (f (S.prefix x) ys) y
  where
    f :: (Stream a -> Stream a) -> OList1 (Stream a) -> OList1 (Stream a)
    f h (End (ws :| wss)) = End (h ws :| wss)
    f h (Power vs v     ) = Power (f (\(Cons w ws) -> Cons (h w) ws) vs) v
{-# INLINE withPrefix #-}

omega :: Stream a -> OList1 a
omega xs = Power (End (xs :| [])) []
{-# INLINE omega #-}

instance Semigroup (OList1 a) where
    End x        <> End y = End (x <> y)
    (Power xs x) <> End y = Power xs (x ++ E.toList y)
    End x        <> y     = withPrefix (E.toList x) y
    (Power xs x) <> y     = let (Power ys' y') = withPrefix x y in Power (xs <> ys') y'

-- | The given function must prepend at least one element to a stream,
-- otherwise the computation diverges.
timesOmega :: (b -> Stream c -> Stream c) -> Stream b -> Stream c
timesOmega f = loop  where
    loop (b `Cons` bs) = f b $ loop bs
    {-# INLINE loop #-}
{-# INLINE timesOmega #-}

timesList :: OList1 (b -> c) -> NonEmpty b -> OList1 c
timesList f ys = sconcat (fmap (\y -> f <&> ($ y)) ys)
{-# INLINE timesList #-}

-- | Applies a carry-over value expressed as function `b -> c -> c` to the
-- first element of a stream, and then prepends elements produced by mapping
-- over a `[a]` with a given function.
--
-- This essentially implements carrying over a "tail" of an ordinal as a
-- function, as we nest in its next `Stream` level.
nestedCarry :: (a -> b -> c) -> [a] -> (b -> c -> c) -> (b -> Stream c -> Stream c)
nestedCarry h xl carry v (z `Cons` zs) = S.prefix (xl <&> (`h` v)) (carry v z `Cons` zs)
{-# INLINE nestedCarry #-}

-- | Multiplication of an ordinal by an infinite ordinal.
--
-- It's implemented by nesting into the first ordinal and converting the
-- ordinal to a function `b -> c -> c` that prepends the respective product
-- values to `c`. Eventually the whole ordinal is converted to such a function
-- and then executed by `timesOmega`.
mul :: forall b c . OList1 (b -> c) -> OList1 (Stream b) -> OList1 (Stream c)
mul x y = loop ($) x (const id)
  where
    loop :: forall a c . (a -> b -> c) -> OList1 a -> (b -> c -> c) -> OList1 (Stream c)
    loop h (Power x xl) carry = Power (loop h' x (nestedCarry h xl carry)) []
        where h' us v = us <&> (`h` v)
    loop h (End xl) carry = timesOmega (nestedCarry h (E.toList xl) carry) <$> y
    {-# INLINE loop #-}
{-# INLINE mul #-}

instance Applicative OList1 where
    pure x = End (x :| [])
    f <*> End ys             = timesList f ys
    f <*> Power y []         = Power (mul f y) []
    f <*> Power y (y' : yl') = Power (mul f y) [] <> timesList f (y' :| yl')
