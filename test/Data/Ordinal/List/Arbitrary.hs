-- Copyright 2021-2023 Google LLC
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

import           Data.Foldable                  ( toList )
import           Data.Function                  ( on )
import           Data.Ordinal.List
import           Data.Sequence                  ( Seq(..) )
import qualified Data.Sequence                 as Q
import qualified Data.Stream                   as S
import           Test.QuickCheck

sampleSize :: (Num a) => a
sampleSize = 3

instance (Arbitrary a) => Arbitrary (OList a) where
    arbitrary = getSize >>= genWith . floor . logBase sampleSize . fromIntegral
      where
        genWith :: (Arbitrary a) => Int -> Gen (OList a)
        genWith 0     = pure <$> arbitrary
        genWith depth = frequency [(1, pure mempty), (4, nested)]
          where
            nested = do
                left <- arbitrary
                (:^> left) <$> genWith (depth - 1)
    -- Shrink by:
    -- - Dropping the infinite part completely.
    -- - Taking only the heads of elements in `xs`.
    -- - Shrinking the tail (finite) part.
    shrink Zero          = []
    shrink (Zero :^> xl) = (Zero :^>) <$> shrink xl
    shrink (xs :^> xl) = [Zero :^> xl, fmap S.head xs <> (Zero :^> xl)] ++ ((xs :^>) <$> shrink xl)

-- | A data type with a structure very similar to `OList`, but using finite
-- lists instead of `Stream`s. This allows printing and comparing test values.
data OListSample a = Zero' | Omega' (OListSample (Seq a)) (Seq a)
  deriving (Eq, Functor, Ord, Show)  -- TODO: Custom Show similar to `OList`'s.

sampleToFinite :: Int -> OList a -> OListSample a
sampleToFinite n (Zero :^> Empty) = Zero'
sampleToFinite n (xs   :^> xl   ) = Omega' (Q.fromList . S.take n <$> sampleToFinite n xs) xl

-- | Compares two `OList` instances up to a certain predefined depth.
infix 4 =~=
(=~=) :: (Eq a, Show a) => OList a -> OList a -> Property
(=~=) = on (===) (sampleToFinite sampleSize)
