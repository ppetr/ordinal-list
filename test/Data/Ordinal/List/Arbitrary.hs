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

import           Data.Ordinal.List
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Q
import qualified Data.Stream                   as S
import           Test.QuickCheck

instance (Arbitrary a) => Arbitrary (OList a) where
    arbitrary = getSize >>= genWith arbitrary
      where
        genWith :: Gen a -> Int -> Gen (OList a)
        genWith f 0 = pure Zero
        genWith f n = frequency
            [ (1, pure Zero)
            , (1, Omega <$> f <*> genWith (genStream f) (n - 1) <*> seqOf f)
            ]
        genStream :: Gen a -> Gen (S.Stream a)
        genStream = fmap S.cycle . listOf1
        seqOf :: Gen a -> Gen (Seq a)
        seqOf = fmap Q.fromList . listOf
    shrink Zero = []
    shrink (Omega x xo xs) =
        Zero : [ Omega x xo' xs' | (xo', xs') <- shrink (xo, xs) ]

-- | A data type with a structure very similar to `OList`, but using finite
-- lists instead of `Stream`s. This allows printing and comparing test values.
data OListSample a = Zero' | Omega' a (OListSample (Seq a)) (Seq a)
  deriving (Eq, Functor, Ord, Show)

sampleToFinite :: Int -> OList a -> OListSample a
sampleToFinite _ Zero = Zero'
sampleToFinite n (Omega x xo xs) =
    Omega' x (Q.fromList . S.take n <$> sampleToFinite n xo) xs

-- | Compares two `OList` instances up to a certain predefined depth.
infix 4 =~=
(=~=) :: (Eq a, Show a) => OList a -> OList a -> Property
xo =~= yo = sampleToFinite 3 xo === sampleToFinite 3 yo
