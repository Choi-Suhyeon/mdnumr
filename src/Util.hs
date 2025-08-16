{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}

module Util 
    ( Vector          , N7
    , StartBase(..)   , Range
    , PrintRange      , pattern RangeView
    , mkRange         , inRange
    , lowerLimit      , upperLimit
    , updateLower     , updateUpper
    , updateLowerWith , updateUpperWith
    , baseToNum
    ) 
  where

import Data.Vector.Unboxed.Sized qualified as UV

import Control.Arrow ((&&&))
import Data.Finite   (Finite)
import Data.Int      (Int32)

type Vector n = UV.Vector n Int32

type N7 = 7

data StartBase = ZeroBased | OneBased

baseToNum :: Num a => StartBase -> a
baseToNum ZeroBased = 0
baseToNum OneBased  = 1

type PrintRange = Range (Finite N7)

data Range a
    = Range 
    { lowerLimit :: a
    , upperLimit :: a
    }

deriving instance Show a => Show (Range a)

pattern RangeView :: a -> a -> Range a
pattern RangeView lo hi <- (lowerLimit &&& upperLimit -> (lo, hi))

{-# COMPLETE RangeView #-}

mkRange :: Ord a => a -> a -> Maybe (Range a)
mkRange x y = 
    case compare x y of
        GT -> Nothing
        _  -> Just $ Range x y

inRange :: Ord a => Range a -> a -> Bool
inRange (Range n m) x = n <= x && x <= m

updateLower :: Ord a => a -> Range a -> Maybe (Range a)
updateLower = updateLowerWith . const

updateUpper :: Ord a => a -> Range a -> Maybe (Range a)
updateUpper = updateUpperWith . const

updateLowerWith :: Ord a => (a -> a) -> Range a -> Maybe (Range a)
updateLowerWith f (Range n m) = 
    case compare n' m of 
        GT -> Nothing
        _  -> Just $ Range n' m
  where
    n' = f n

updateUpperWith :: Ord a => (a -> a) -> Range a -> Maybe (Range a)
updateUpperWith f (Range n m) = 
    case compare n m' of 
        GT -> Nothing
        _  -> Just $ Range n m'
  where
    m' = f m

