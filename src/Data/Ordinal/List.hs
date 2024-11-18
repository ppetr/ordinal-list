-- Copyright 2021-2024 Google LLC
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

{-# LANGUAGE DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Data.Ordinal.List
    ( OList()
    , isFinite
    , fromFinite
    , fromSeq
    , fromStream
    , wrapStream
    , omega
    , namedOrdinals
    , VonNeumann
    , vonNeumann
    ) where

import           Control.Applicative
import           Data.Foldable                  ( toList )
import           Data.List                      ( intersperse )
import qualified Data.List.NonEmpty            as E
import           Data.Monoid                    ( Endo(..) )
import           Data.Ordinal.NonEmpty          ( OList1() )
import qualified Data.Ordinal.NonEmpty         as N
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Q
import           Data.Stream                    ( Stream(Cons) )
import qualified Data.Stream                   as S

-- | Represents a list indexed by ordinals < ω^ω.
newtype OList a = OList (Maybe (OList1 a))
  deriving (Functor, Monoid, Semigroup)

-- | Prints a finite fragment of an ordinal list.
instance (Show a) => Show (OList a) where
    showsPrec _ (OList Nothing ) = showString "<>"
    showsPrec _ (OList (Just x)) = shows x

isFinite :: OList a -> Maybe (Seq a)
isFinite (OList Nothing ) = Just Q.Empty
isFinite (OList (Just x)) = N.isFinite x

fromSeq :: Seq a -> OList a
fromSeq Q.Empty      = empty
fromSeq (x Q.:<| xl) = OList (Just $ N.fromSeq x xl)
{-# INLINE fromSeq #-}

fromFinite :: (Foldable f) => f a -> OList a
fromFinite xl | (x : xl') <- toList xl = OList (Just $ N.fromNonEmpty (x E.:| xl'))
              | otherwise              = mempty

fromStream :: Stream a -> OList a
fromStream = OList . Just . N.fromStream

wrapStream :: OList (Stream a) -> OList a
wrapStream (OList (Just x)) = OList . Just . N.wrapStream $ x
wrapStream (OList Nothing ) = OList Nothing

omega :: OList Integer
omega = OList (Just N.omega)

instance Applicative OList where
    pure = OList . Just . pure
    (OList Nothing)  <*> _                = empty
    _                <*> (OList Nothing ) = empty
    (OList (Just x)) <*> (OList (Just y)) = OList (Just $ x <*> y)

instance Alternative OList where
    empty = mempty
    (<|>) = (<>)

-- There is no `Monad` instance for `OList`.
--
-- Consider expression `omega >>= \_ -> Zero`. The result would be `Zero`, but
-- it's not possible to conclude this in a finite number of steps.
--
-- This argument can be brought further: Let
--
--     h = omega >>= \n -> if <program P finishes after 'n' steps>
--                         then one else Zero
--
-- Then `h` equals `Zero` iff program P runs indefinitely, thus solving the
-- halting problem.

-- * Set theory

-- | Stream of ordinals, where the `i`-th element is an ordinal ω^i containing
-- human representation of all ordinals below it.
--
-- The stream yields all ordinals representable by the `OList` type.
namedOrdinals :: Stream (OList String)
namedOrdinals = pure "0" `Cons` S.zipWith f (S.iterate (+ 1) 0) namedOrdinals
  where
    f n o = (\s i -> sum $ power i n ++ [ s | s /= "0" ]) <$> o <*> omega
    power 0 _ = []
    power i 0 = [show i]
    power 1 1 = [o]
    power 1 j = [o ++ "^" ++ show j]
    power i 1 = [show i ++ o]
    power i j = [show i ++ o ++ "^" ++ show j]
    o = "\x03C9" :: String
    sum [] = "0"
    sum xs = mconcat . intersperse "+" $ xs

-- | Experimental, not really tested if the functions below compute correctly.
newtype VonNeumann = VonNeumann (OList VonNeumann)

instance Semigroup VonNeumann where
    x@(VonNeumann x') <> VonNeumann y = VonNeumann (x' <> fmap (x <>) y)

instance Monoid VonNeumann where
    mempty = VonNeumann mempty

instance Show VonNeumann where
    showsPrec _ (VonNeumann o) = showString "vN" . shows o

-- | An infinite stream of von Neumann ordinals 1, ω, ω^2, ...
vonNeumann :: Stream VonNeumann
vonNeumann = VonNeumann (pure mempty) `Cons` fmap timesOmega vonNeumann
  where
    timesOmega :: VonNeumann -> VonNeumann
    timesOmega o = VonNeumann . fromStream $ S.iterate (o <>) o
