-- Copyright 2024 Google LLC
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -W #-}

module Data.Ordinal.NonEmpty
  ( OList1 (..),
    withPrefix,
    fromNonEmpty,
    fromSeq,
    fromStream,
    wrapStream,
    omega,
    isFinite,
    head1,
    OListOrdering (..),
    ordinalOrdering,
    zipSplit,
  )
where

import Control.Arrow (first)
import Data.Foldable (toList)
import Data.Functor ((<&>))
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
import Data.Sequence
  ( Seq (..),
    (<|),
  )
import qualified Data.Sequence as Q
import Data.Stream (Stream (Cons))
import qualified Data.Stream as S

-- | Non-empty ordinal-indexed list up to ω^ω.
data OList1 a = Finite a (Seq a) | Power !(OList1 (Stream a)) (Seq a)
  deriving (Functor)

-- | Prepends a given prefix to an ordinal.
withPrefix :: Seq a -> OList1 a -> OList1 a
withPrefix Empty y = y
withPrefix (x :<| xl) (Finite y yl) = Finite x (xl <> (y <| yl))
withPrefix x (Power ys y) = Power (runIdentity $ head1 (Identity . S.prefix (toList x)) ys) y
{-# INLINE withPrefix #-}

-- | Van Laarhoven lens for accessing the first element.
head1 :: (Functor f) => (a -> f a) -> OList1 a -> f (OList1 a)
head1 f (Finite x xl) = f x <&> (`Finite` xl)
head1 f (Power x xl) = head1 (\ ~(v `Cons` vs) -> f v <&> (`Cons` vs)) x <&> (`Power` xl)
{-# INLINEABLE head1 #-}

instance Semigroup (OList1 a) where
  Finite x xl <> Finite y yl = Finite x (xl <> (y <| yl))
  Power xs x <> Finite y yl = Power xs (x <> (y <| yl))
  Finite x xl <> y = withPrefix (x <| xl) y
  Power xs x <> y =
    let (Power ys' y') = withPrefix x y
     in Power (xs <> ys') y'

-- | The given function must prepend at least one element to a stream,
-- otherwise the computation diverges.
timesOmega :: (b -> Stream c -> Stream c) -> Stream b -> Stream c
timesOmega f = loop
  where
    loop ~(b `Cons` bs) = f b $ loop bs
    {-# INLINE loop #-}
{-# INLINE timesOmega #-}

timesList :: OList1 (b -> c) -> NonEmpty b -> OList1 c
timesList f ys = sconcat (fmap (\y -> f <&> ($ y)) ys)
{-# INLINE timesList #-}

-- | Applies a carry-over value expressed as function `b -> c -> c` to the
-- first element of a stream, and then prepends elements produced by mapping
-- over a `[a]` with a given function.
--
-- This essentially implements carrying over a "tail" of an ordinal as a
-- function, as we nest in its next `Stream` level.
nestedCarry :: (a -> b -> c) -> Seq a -> (b -> c -> c) -> (b -> Stream c -> Stream c)
-- Note that the match on the last argument `Stream c` must be lazy so that the
-- prefix is prepended before the tail is examined. Without it an infinite loop
-- occurs.
nestedCarry h xl carry v ~(z `Cons` zs) = S.prefix (toList xl <&> (`h` v)) (carry v z `Cons` zs)
{-# INLINE nestedCarry #-}

-- | Multiplication of an ordinal by an infinite ordinal.
--
-- It's implemented by nesting into the first ordinal and converting the
-- ordinal to a function `b -> c -> c` that prepends the respective product
-- values to `c`. Eventually the whole ordinal is converted to such a function
-- and then executed by `timesOmega`.
mul :: forall b c. OList1 (b -> c) -> OList1 (Stream b) -> OList1 (Stream c)
mul x0 y = loop ($) x0 (const id)
  where
    loop :: forall a c. (a -> b -> c) -> OList1 a -> (b -> c -> c) -> OList1 (Stream c)
    loop h (Power x xl) carry = Power (loop h' x (nestedCarry h xl carry)) Empty
      where
        h' us v = us <&> (`h` v)
    loop h (Finite x xl) carry = timesOmega (nestedCarry h (x <| xl) carry) <$> y
    {-# INLINE loop #-}
{-# INLINE mul #-}

instance Applicative OList1 where
  pure x = Finite x Empty
  f <*> Finite y ys = timesList f (y :| toList ys)
  f <*> Power y Empty = Power (mul f y) Empty
  f <*> Power y (y' :<| yl') = Power (mul f y) Empty <> timesList f (y' :| toList yl')

-- * Other instances.

-- | Prints a finite fragment of a list.
instance (Show a) => Show (OList1 a) where
  showsPrec _ = showsOList . fmap shows
    where
      showsOList :: OList1 ShowS -> ShowS
      showsOList (Finite x xl) = showString "<" . x . append xl . showString ">"
      showsOList (Power x xl) =
        showString "<" . showsOList (prefixOf <$> x) . append xl . showString ">"
      prefixOf :: Stream ShowS -> ShowS
      prefixOf ~(x `Cons` xs) = showString "[" . x . append (S.take 3 xs) . showString ",...]"
      append :: (Foldable f) => f ShowS -> ShowS
      append = appEndo . foldMap (\x -> Endo (showString "," . x))

-- * Construction

fromNonEmpty :: NonEmpty a -> OList1 a
fromNonEmpty (x :| xl) = fromSeq x (Q.fromList xl)
{-# INLINE fromNonEmpty #-}

fromSeq :: a -> Seq a -> OList1 a
fromSeq = Finite
{-# INLINE fromSeq #-}

fromStream :: Stream a -> OList1 a
fromStream = wrapStream . pure
{-# INLINE fromStream #-}

wrapStream :: OList1 (Stream a) -> OList1 a
wrapStream x = Power x Empty
{-# INLINEABLE wrapStream #-}

omega :: OList1 Integer
omega = fromStream (S.iterate (+ 1) 0)
{-# INLINEABLE omega #-}

-- * Inspection

-- | If a given ordinal is finite, return it as a (non-empty) sequence.
-- Otherwise return `Nothing`.
isFinite :: OList1 a -> Maybe (Seq a)
isFinite (Finite x xl) = Just (x <| xl)
isFinite _ = Nothing
{-# INLINEABLE isFinite #-}

-- | Represents the result of zipping/comparing two ordinals and holds the
-- remainder of the greater one of the two.
data OListOrdering a b = OListLT (OList1 b) | OListEQ | OListGT (OList1 a)
  deriving (Show)

ordinalOrdering :: OListOrdering a b -> Ordering
ordinalOrdering OListEQ = EQ
ordinalOrdering (OListLT _) = LT
ordinalOrdering (OListGT _) = GT

stripPrefix :: Int -> OList1 (Stream a) -> (Seq a, OList1 (Stream a))
stripPrefix o = head1 (first Q.fromList . S.splitAt o)
{-# INLINE stripPrefix #-}

zipPrefix :: (a -> b -> c) -> a -> Seq a -> OList1 (Stream b) -> (c, Seq c, OList1 (Stream b))
zipPrefix f x xl o =
  let (y :<| yl, o') = stripPrefix (Q.length xl + 1) o
   in (f x y, Q.zipWith f xl yl, o')
{-# INLINE zipPrefix #-}

compareFinite :: Seq a -> Seq b -> (Seq (a, b), OListOrdering a b)
compareFinite xl yl =
  let m = min (Q.length xl) (Q.length yl)
      xl' = Q.drop m xl
      yl' = Q.drop m yl
      z = Q.zip xl yl
   in case (xl', yl') of
        (u :<| us, _) -> (z, OListGT (fromSeq u us))
        (_, v :<| vs) -> (z, OListLT (fromSeq v vs))
        _ -> (z, OListEQ)
{-# INLINE compareFinite #-}

-- | See `zipSplit` in the `Lists` module.
zipSplit :: OList1 a -> OList1 b -> (OList1 (a, b), OListOrdering a b)
zipSplit (Finite x xl) (Finite y yl)
  | (z :<| zl, o) <- compareFinite (x <| xl) (y <| yl) = (fromSeq z zl, o)
  | otherwise = error "Can't happen - input sequences are both non-empty"
zipSplit (Power x xl) (Finite y yl) =
  let (z, zl, x') = zipPrefix (flip (,)) y yl x
   in (fromSeq z zl, OListGT (Power x' xl))
zipSplit (Finite x xl) (Power y yl) =
  let (z, zl, y') = zipPrefix (,) x xl y
   in (fromSeq z zl, OListLT (Power y' yl))
zipSplit (Power x xl) (Power y yl) = case zipSplit x y of
  (z, OListEQ) -> case compareFinite xl yl of
    (zl, o) -> (Power (fmap (uncurry S.zip) z) zl, o) -- TODO xl yl
  (z, OListLT y') | Empty <- xl -> (Power (fmap (uncurry S.zip) z) Empty, OListLT (Power y' yl))
  (z, OListLT y')
    | xl1 :<| xl' <- xl,
      (z1', zl', y'') <- zipPrefix (,) xl1 xl' y' ->
        (Power (fmap (uncurry S.zip) z) (z1' :<| zl'), OListLT (Power y'' yl))
  (z, OListGT x') | Empty <- yl -> (Power (fmap (uncurry S.zip) z) Empty, OListGT (Power x' xl))
  (z, OListGT x')
    | yl1 :<| yl' <- yl,
      (z1', zl', x'') <- zipPrefix (flip (,)) yl1 yl' x' ->
        (Power (fmap (uncurry S.zip) z) (z1' :<| zl'), OListGT (Power x'' xl))
