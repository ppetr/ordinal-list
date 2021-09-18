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
module Data.Ordinal.List.Arbitrary where

import Data.Ordinal.List
import qualified Data.Stream as S
import Test.QuickCheck

instance (Arbitrary a) => Arbitrary (OList a) where
    arbitrary = frequency
        [ (1, pure Zero)
        , (5, Omega <$> arbitrary <*> arbitrary <*> arbitrary)
        ]
    shrink Zero = []
    shrink (Omega x xo xs)
        = [Omega x Zero xs] ++
          [Omega x' xo' xs' | (x', xo', xs') <- shrink (x, xo, xs)]

-- | A data type with a structure very similar to `OList`, but using finite
-- lists instead of `Stream`s. This allows printing and comparing test values.
data OListSample a = Zero' | Omega' a (OListSample [a]) [a]
  deriving (Eq, Functor, Ord, Show)

sampleToFinite :: Int -> OList a -> OListSample a
sampleToFinite _ Zero = Zero'
sampleToFinite n (Omega x xo xs)
    = Omega' x (S.take n <$> sampleToFinite n xo) xs

-- | Compares two `OList` instances up to a certain predefined depth.
(=~=) :: (Eq a, Show a) => OList a -> OList a -> Property
xo =~= yo = sampleToFinite 5 xo === sampleToFinite 5 yo
