{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}

{- |

Description: utilities for working with Maps, with explicit (Monad)Errors
             when reading Lists with duplicate keys, etc.

-}

module ContainersPlus.MapUtils
  ( MapDupKeyError( MapDupKeyError )
  , fromListWithDups, fromListDupsE, invertMap, invertMapS, mapFromList
  , mergeMaps
  )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Control.Monad        ( return )
import Data.Bifunctor       ( second )
import Data.Either          ( Either( Right, Left ) )
import Data.Eq              ( Eq )
import Data.Foldable        ( Foldable, concat )
import Data.Function        ( ($) )
import Data.Functor         ( Functor, (<$>) )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Monoid          ( (<>) )
import Data.Ord             ( Ord )
import Data.Tuple           ( swap )
import GHC.Exts             ( IsList( toList ), Item )
import Text.Show            ( Show )

-- containers --------------------------

import Data.Map  ( Map, fromListWith, mapEither, null )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import NonEmptyContainers.NonEmptyHashSet ( NonEmptyHashSet
                                          , fromList, singleton )

-------------------------------------------------------------------------------

-- I'd really like to make the type of the NonEmptyHashSet imply that it must
-- be of size 2 or more.  A job for another day.

data MapDupKeyError k v = MapDupKeyError (Map k (NonEmptyHashSet v))
  deriving (Eq, Show)

------------------------------------------------------------

mapFromList :: (Ord k, Hashable v, Eq v, IsList l, Item l ~ (k,v)) =>
             l -> Map k (NonEmptyHashSet v)
mapFromList kvs = fromListWith (<>) (second singleton <$> toList kvs)

----------------------------------------

-- | Construct a map from a list of pairs, along with a list of keys with more
--   than one value (c.f., `Data.Map.fromList`, which discards duplicates).
--   The lists have disjoint keys - the 'normal' list contains only those keys
--   that don't have duplicate values.

fromListWithDups :: (Ord k, Eq v, Hashable v, IsList l, Item l ~ (k,v)) =>
                    l -> (Map k (NonEmptyHashSet v), Map k v)
fromListWithDups ls =
  let map = fromListWith (<>) (second pure <$> toList ls)
   in mapEither ( \ case (v :| []) -> Right v; vs -> Left (fromList vs) )
                map

----------------------------------------

-- | like `fromListWithDups`, but instead of return both lists with & without
--   dups, this one returns the 'normal' list if there are no dups; else throws
--   the duplist

fromListDupsE :: (Ord k, Eq v, Hashable v, IsList l, Item l ~ (k,v),
                  MonadError (MapDupKeyError k v) m) =>
                 l -> m (Map k v)
fromListDupsE ls =
  let (dups, res) = fromListWithDups ls
   in if null dups
      then return res
      else throwError $ MapDupKeyError dups

----------------------------------------

-- | invert a map, that is, Map v k -> Map k v; throw an error if duplicate
--   k values are found

invertMap :: (Ord k, Eq v, Hashable v, IsList l, Item l ~ (v,k),
              MonadError (MapDupKeyError k v) m) =>
             l -> m (Map k v)
invertMap m = fromListDupsE (swap <$> toList m)

----------------------------------------

-- | invert a map which is a map to a list of values, that is,
--   Map v [k] -> Map k v; throw an error if duplicate k values are found

invertMapS :: (Ord k, Eq v, Hashable v, IsList l, Item l ~ (v,[k]),
               MonadError (MapDupKeyError k v) m) =>
              l -> m (Map k v)
invertMapS m = fromListDupsE $ [ (v,k) | (k,vs) <- toList m, v <- vs ]

----------------------------------------

-- | merge a list of maps, throw if any key is duplicated

mergeMaps :: (Ord k, Eq v, Hashable v, MonadError (MapDupKeyError k v) m,
              Foldable t, Functor t) =>
             t (Map k v) -> m (Map k v)
mergeMaps maps = fromListDupsE (concat $ toList <$> maps)

-- that's all, folks! ---------------------------------------------------------
