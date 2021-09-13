-- Copyright 2021 Google LLC
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
module Lib
    ( NonEmpty(...)
    ) where

import Control.Applicative
import Data.List.NonEmpty as L
import Data.Semigroup
import Data.Stream (Stream, Cons)
import Data.Stream as S
import Prelude hiding (head, tail)

-- | Represents a non-empty list indexed by ordinals < ω^ω.
data NonZero a 
    = Finite (NonEmpty a)
    | Omega (NonZero (Stream a)) [a]
  deriving (Functor)

onFirst :: (a -> a) -> NonZero a -> NonZero a
onFirst f (Finite (x :| xs)) = Finite (f x :| xs)
onFirst f (Omega xo xs) = Omega (onFirst (\(Cons u us) -> Cons (f u) us) xo) xs

prependFinite :: (Foldable f) => f a -> NonZero a -> NonZero a
prependFinite [] ys = ys
prependFinite (x : xs) (Finite ys) = Finite ((x :| xs) <> ys)
