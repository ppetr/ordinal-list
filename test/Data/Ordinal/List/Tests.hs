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

{-# LANGUAGE TemplateHaskell #-}
module Data.Ordinal.List.Tests where

import Data.Maybe (isNothing)
import Data.Ordinal.List
import Data.Ordinal.List.Arbitrary
import Test.QuickCheck
import Test.Tasty.QuickCheck as QC
import Test.Tasty (TestTree)

prop_omega_infinite :: Property
prop_omega_infinite = once (isNothing $ isFinite omega)

prop_list_monomorphism :: (Arbitrary a, Eq a, Show a) => [a] -> Property
prop_list_monomorphism xs = Just xs === isFinite (fromList xs)

prop_semigroup :: (Arbitrary a, Eq a, Show a)
               => OList a -> OList a -> OList a -> Property
prop_semigroup x y z = (x <> y) <> z =~= x <> (y <> z)

return []

tests :: TestTree
tests = testProperties "Data.Ordinal.List" $allProperties
