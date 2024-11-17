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
    , fromStream
    , omega
    ) where

import           Control.Applicative
import           Data.Foldable                  ( toList )
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
    showsPrec _ (OList Nothing ) = showString "[]"
    showsPrec _ (OList (Just x)) = shows x

isFinite :: OList a -> Maybe (Seq a)
isFinite (OList Nothing ) = Just Q.Empty
isFinite (OList (Just x)) = N.isFinite x

fromFinite :: (Foldable f) => f a -> OList a
fromFinite xl | (x : xl') <- toList xl = OList (Just $ N.fromNonEmpty (x E.:| xl'))
              | otherwise              = mempty

fromStream :: Stream a -> OList a
fromStream = OList . Just . N.fromStream

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
