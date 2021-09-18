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

{-# LANGUAGE DeriveFunctor, PatternSynonyms #-}
module Data.Ordinal.List
    ( OList
    , isFinite
    , fromList
    , fromStream
    , omega
    ) where

import Control.Applicative
import Data.Stream (Stream(Cons))
import qualified Data.Stream as S
import Prelude hiding (head, tail)

-- | Represents a list indexed by ordinals < ω^ω.
data OList a
    = Zero
    | Omega a (OList (Stream a)) [a] -- ^ 1 + ω*α + n
  deriving (Functor)

isFinite :: OList a -> Maybe [a]
isFinite Zero = Just []
isFinite (Omega x Zero xs) = Just (x : xs)
isFinite _ = Nothing

-- | The input list must be finite, otherwise the ordinal will have a malformed
-- structure and operations on it will diverge. Use `fromStream` to construct a
-- correct, infinite one.
fromList :: [a] -> OList a
fromList [] = Zero
fromList (x : xs) = Omega x Zero xs

fromStream :: Stream a -> OList a
fromStream (x `Cons` xs) = Omega x (Omega xs Zero []) []

omega :: OList Integer
omega = fromStream (S.iterate (+ 1) 0)

instance Semigroup (OList a) where
    Zero <> yo = yo
    xo <> Zero = xo
    Omega x xo xs <> Omega y Zero ys
        = Omega x xo (xs ++ (y : ys))
    Omega x xo xs <> Omega y (Omega y' yo ys') ys
        = Omega x (xo <> Omega (foldr Cons (y `Cons` y') xs) yo ys') ys

instance Monoid (OList a) where
    mempty = Zero

instance Applicative OList where
    pure x = Omega x Zero []
    (<*>) = undefined

instance Alternative OList where
    empty = Zero
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
