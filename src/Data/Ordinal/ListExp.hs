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
    ( Ordinal(..)
    , withPrefix
    ) where

import           Data.Functor                   ( (<&>) )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Data.List.NonEmpty            as E
import           Data.Semigroup
import           Data.Stream                    ( Stream(Cons) )
import qualified Data.Stream                   as S

-- TODO: Use a strict-spine `NonEmpty` list if possible in `End` to enforce
-- that it's never infinite.
data Ordinal a = End (NonEmpty a) | Power !(Ordinal (Stream a)) [a]
  deriving (Functor)

withPrefix :: [a] -> Ordinal a -> Ordinal a
withPrefix []       y            = y
withPrefix (x : xl) (End y     ) = End ((x :| xl) <> y)
withPrefix x        (Power ys y) = Power (f (S.prefix x) ys) y
  where
    f :: (Stream a -> Stream a) -> Ordinal (Stream a) -> Ordinal (Stream a)
    f h (End (ws :| wss)) = End (h ws :| wss)
    f h (Power vs v     ) = Power (f (\(Cons w ws) -> Cons (h w) ws) vs) v

instance Semigroup (Ordinal a) where
    End x        <> End y = End (x <> y)
    (Power xs x) <> End y = Power xs (x ++ E.toList y)
    End x        <> y     = withPrefix (E.toList x) y
    (Power xs x) <> y     = let (Power ys' y') = withPrefix x y in Power (xs <> ys') y'

timesOmega :: (a -> b -> c) -> NonEmpty a -> (b -> c -> c) -> Stream b -> Stream c
timesOmega h xl carry = loop
  where
    carry' b (c `Cons` cs) = carry b c `Cons` cs
    xl' = E.toList xl
    loop (b `Cons` bs) = S.prefix (xl' <&> (`h` b)) . carry' b $ loop bs

timesList :: Ordinal (b -> c) -> NonEmpty b -> Ordinal c
timesList f ys = sconcat (fmap (\y -> f <&> ($ y)) ys)

mul :: forall b c . Ordinal (b -> c) -> Ordinal (Stream b) -> Ordinal (Stream c)
mul x y = loop ($) x (const id)
  where
    loop :: forall a c . (a -> b -> c) -> Ordinal a -> (b -> c -> c) -> Ordinal (Stream c)
    loop h (Power x xl) carry = Power (loop h' x carry') []
      where
        h' us v = us <&> (`h` v)
        carry' v (z `Cons` zs) = S.prefix (map (`h` v) xl) (carry v z `Cons` zs)
    loop h (End xl) carry = timesOmega h xl carry <$> y

instance Applicative Ordinal where
    pure x = End (x :| [])
    f <*> End ys             = timesList f ys
    f <*> Power y []         = Power (mul f y) []
    f <*> Power y (y' : yl') = Power (mul f y) [] <> timesList f (y' :| yl')
