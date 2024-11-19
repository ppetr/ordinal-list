-- Copyright 2023-2024 Google LLC
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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Experimental, standalone module for defining ordinals using type-level
-- Church encodings.
module Data.Ordinal.Church where

import Data.Functor.Identity

-- A type-level Church ordinal is a type of kind '(* -> *) -> * -> *' that
-- applies the first argument α times.

type Succ a f t = a f (f t)

type (a :+ b) f t = b f (a f t)

type Mul a b f = a (b f)

data Finite f a = Zero | One a !(Finite f (f a))
  deriving (Functor)

data Limit f a = Limit a (Limit f (f a))
  deriving (Functor)

newtype Omega' a = Omega' (Limit Identity a)
  deriving (Functor)

newtype Omega a = Omega (Finite Identity a)
  deriving (Functor)

newtype OmegaToOmega' a = OmegaToOmega' (Limit Omega a)
  deriving (Functor)

newtype OmegaToOmega a = OmegaToOmega (Finite Omega a)
  deriving (Functor)

instance Applicative OmegaToOmega where
  pure x = OmegaToOmega $ One x Zero

-- TODO: Validate if this actually defines epsilon0. That is, if it's the
-- (smalled) fixed point of ω^.
newtype Epsilon1 a = Epsilon1 (Limit OmegaToOmega a)

type OrdinalK = forall k. (k -> k) -> k -> k
