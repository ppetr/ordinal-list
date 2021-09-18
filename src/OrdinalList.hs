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
module OrdinalList
    ( Ordinal
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
data Ordinal a
    = Zero
    | Omega a (Ordinal (Stream a)) [a] -- ^ 1 + ω*α + n
  deriving (Functor)

isFinite :: Ordinal a -> Maybe [a]
isFinite Zero = Just []
isFinite (Omega x Zero xs) = Just (x : xs)
isFinite _ = Nothing

-- | The input list must be finite, otherwise the ordinal will have a malformed
-- structure and operations on it will diverge. Use `fromStream` to construct a
-- correct, infinite one.
fromList :: [a] -> Ordinal a
fromList [] = Zero
fromList (x : xs) = Omega x Zero xs

fromStream :: Stream a -> Ordinal a
fromStream (x `Cons` xs) = Omega x (Omega xs Zero []) []

omega :: Ordinal Integer
omega = fromStream (S.iterate (+ 1) 0)

instance Semigroup (Ordinal a) where
    Zero <> yo = yo
    xo <> Zero = xo
    Omega x xo xs <> Omega y Zero ys
        = Omega x xo (xs ++ (y : ys))
    Omega x xo xs <> Omega y (Omega y' yo ys') ys
        = Omega x (xo <> Omega (foldr Cons (y `Cons` y') xs) yo ys') ys

instance Monoid (Ordinal a) where
    mempty = Zero

instance Applicative Ordinal where
    pure x = Omega x Zero []
    (<*>) = undefined

instance Alternative Ordinal where
    empty = Zero
    (<|>) = (<>)

-- There is no `Monad` instance for `Ordinal`.
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
