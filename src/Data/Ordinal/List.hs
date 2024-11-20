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
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Ordinal.List
  ( OList ((:^>), NonEmpty, Zero),
    fromFinite,
    fromStream,
    omega,
    N.OListOrdering (..),
    zipSplit,
    namedOrdinals,
    VonNeumann,
    vonNeumann,
  )
where

import Control.Applicative
import Control.Arrow (first)
import Data.Foldable (toList)
import Data.List (intersperse)
import Data.Ordinal.NonEmpty (OList1 ())
import qualified Data.Ordinal.NonEmpty as N
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Q
import Data.Stream (Stream (Cons))
import qualified Data.Stream as S

-- | Represents a list indexed by ordinals < ω^ω.
newtype OList a = OList (Maybe (OList1 a))
  deriving (Functor, Monoid, Semigroup)

-- | Witnesses that an ordinal can be uniquely decomposed to ω⋅γ+η where η is
-- finite. And since we only represent ordinals below ω^ω, repeating this
-- decomposition always reaches a point at which the first argument becomes `Zero`.
pattern (:^>) :: OList (Stream a) -> Seq a -> OList a
pattern limit :^> xl <-
  (decompose -> ~(limit, xl))
  where
    (:^>) = compose

{-# COMPLETE (:^>) #-}

pattern Zero :: OList a
pattern Zero <-
  OList Nothing
  where
    Zero = OList Nothing

pattern NonEmpty :: OList1 a -> OList a
pattern NonEmpty x <-
  OList (Just x)
  where
    NonEmpty x = OList (Just x)

{-# COMPLETE Zero, NonEmpty #-}

compose :: OList (Stream a) -> Seq a -> OList a
compose (OList (Just xs)) xl = OList (Just (N.Power xs xl))
compose (OList Nothing) (x :<| xl) = OList (Just (N.Finite x xl))
compose (OList Nothing) Empty = OList Nothing

decompose :: OList a -> (OList (Stream a), Seq a)
decompose (OList Nothing) = (mempty, Empty)
decompose (OList (Just (N.Finite x xl))) = (mempty, x Q.<| xl)
decompose (OList (Just (N.Power xs xl))) = (OList (Just xs), xl)

-- | Prints a finite fragment of an ordinal list.
instance (Show a) => Show (OList a) where
  showsPrec _ (OList Nothing) = showString "<>"
  showsPrec _ (OList (Just x)) = shows x

fromFinite :: (Foldable f) => f a -> OList a
fromFinite = (mempty :^>) . Q.fromList . toList

fromStream :: Stream a -> OList a
fromStream = (:^> Empty) . pure

omega :: OList Integer
omega = OList (Just N.omega)

instance Applicative OList where
  pure = OList . Just . pure
  (OList Nothing) <*> _ = empty
  _ <*> (OList Nothing) = empty
  (OList (Just x)) <*> (OList (Just y)) = OList (Just $ x <*> y)

instance Alternative OList where
  empty = mempty
  (<|>) = (<>)

-- * Minus operation.

-- | Witnesses the "ordinal" property ordinals: Given two ordinals, they're
-- either equal, or one is a prefix of another. In such a case, the remainder
-- is also an ordinal, which implements ordinal subtraction.
zipSplit :: OList a -> OList b -> (OList (a, b), N.OListOrdering a b)
zipSplit Zero Zero = (Zero, N.OListEQ)
zipSplit Zero (NonEmpty y) = (Zero, N.OListLT y)
zipSplit (NonEmpty x) Zero = (Zero, N.OListGT x)
zipSplit (NonEmpty x) (NonEmpty y) = first NonEmpty $ N.zipSplit x y

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
namedOrdinals = pure "0" `Cons` S.zipWith f (S.iterate (+ 1) (0 :: Integer)) namedOrdinals
  where
    f n x = (\s i -> plus $ power i n ++ [s | s /= "0"]) <$> x <*> omega
    power 0 _ = []
    power i 0 = [show i]
    power 1 1 = [o]
    power 1 j = [o ++ "^" ++ show j]
    power i 1 = [show i ++ o]
    power i j = [show i ++ o ++ "^" ++ show j]
    o = "\x03C9" :: String
    plus [] = "0"
    plus xs = mconcat . intersperse "+" $ xs

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
