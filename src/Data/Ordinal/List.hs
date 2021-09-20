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
    ( OList(..)  -- TODO: Move constructors into an internal package.
    , isFinite
    , fromFinite
    , fromSeq
    , fromStream
    , omega
    ) where

import           Control.Applicative
import           Data.Monoid                    ( Endo(..) )
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Q
import           Data.Stream                    ( Stream(Cons) )
import qualified Data.Stream                   as S
import           Prelude                 hiding ( head
                                                , tail
                                                )

-- | Represents a list indexed by ordinals < ω^ω.
data OList a
    = Zero
    | Omega a (OList (Stream a)) (Seq a) -- ^ 1 + ω*α + n
  deriving (Functor)

-- | Prints a finite fragment of an ordinal list.
instance (Show a) => Show (OList a) where
    showsPrec _ = showsOList . fmap shows
      where
        showsOList Zero = showString "[]"
        showsOList (Omega x Zero xt) =
            showString "[" . x . append xt . showString "]"
        showsOList (Omega x (Omega xs xo xts) xt) =
            showString "["
                . showsOList (prefixOf <$> Omega (x `Cons` xs) xo xts)
                . append xt
                . showString "]"
        prefixOf (x `Cons` xs) =
            showString "[" . x . append (S.take 3 xs) . showString ",...]"
        append :: (Foldable f) => f ShowS -> ShowS
        append = appEndo . foldMap (\x -> Endo (showString "," . x))

isFinite :: OList a -> Maybe (Seq a)
isFinite Zero              = Just Empty
isFinite (Omega x Zero xs) = Just (x :<| xs)
isFinite _                 = Nothing

fromFinite :: (Foldable f) => f a -> OList a
fromFinite = fromSeq . foldMap Q.singleton

fromSeq :: Seq a -> OList a
fromSeq Empty      = Zero
fromSeq (x :<| xs) = Omega x Zero xs

fromStream :: Stream a -> OList a
fromStream (x `Cons` xs) = Omega x (Omega xs Zero Empty) Empty

omega :: OList Integer
omega = fromStream (S.iterate (+ 1) 0)

instance Semigroup (OList a) where
    Zero          <> yo              = yo
    xo            <> Zero            = xo
    Omega x xo xs <> Omega y Zero ys = Omega x xo (xs <> (y :<| ys))
    Omega x xo xs <> Omega y (Omega y' yo ys') ys =
        Omega x (xo <> Omega (foldr Cons (y `Cons` y') xs) yo ys') ys

instance Monoid (OList a) where
    mempty = Zero

instance Applicative OList where
    pure x = Omega x Zero Empty
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
