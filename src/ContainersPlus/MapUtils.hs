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

import Base1

-- base --------------------------------

import qualified  GHC.Exts  as  IsList

import Data.Foldable  ( Foldable, concat )
import Data.Functor   ( Functor )
import Data.Tuple     ( swap )

-- containers --------------------------

import qualified  Data.Map  as  Map
import Data.Map  ( Map, fromListWith, mapEither, null )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  NonEmptyContainers.NonEmptyHashSet  as  NonEmptyHashSet
import NonEmptyContainers.NonEmptyHashSet ( NonEmptyHashSet )

-------------------------------------------------------------------------------

-- I'd really like to make the type of the NonEmptyHashSet imply that it must
-- be of size 2 or more.  A job for another day.

data MapDupKeyError κ ν = MapDupKeyError (Map κ (NonEmptyHashSet ν)) CallStack
  deriving Show

instance (Typeable κ, Typeable ν, Show κ, Show ν) ⇒
         Exception (MapDupKeyError κ ν)

instance (Eq κ, Eq ν) ⇒ Eq (MapDupKeyError κ ν) where
  MapDupKeyError m _ == MapDupKeyError m' _ = m == m'

----------

instance (Printable κ, Printable ν) ⇒ Printable (MapDupKeyError κ ν) where
  print (MapDupKeyError m _) =
    let xs ∷ [𝕋]
        xs =  [ [fmt|'%T' ⇒ %T|] k v | (k,v) ← Map.toList m]
     in P.text $ [fmt|duplicate keys: {%L}|] xs

----------

instance HasCallstack (MapDupKeyError κ ν) where
  callstack = lens (\ (MapDupKeyError _ cs) → cs)
                   (\ (MapDupKeyError m _) cs → (MapDupKeyError m cs))

------------------------------------------------------------

class AsMapDupKeyError κ ν ε where
  _MapDupKeyError ∷ Prism' ε (MapDupKeyError κ ν)

instance AsMapDupKeyError κ ν (MapDupKeyError κ ν) where
  _MapDupKeyError = id

------------------------------------------------------------

mapFromList ∷ (Ord κ, Hashable ν, Eq ν, IsList l, Item l ~ (κ,ν)) ⇒
              l → Map κ (NonEmptyHashSet ν)
mapFromList kvs =
  fromListWith (◇) (second NonEmptyHashSet.singleton ⊳ IsList.toList kvs)

----------------------------------------

-- | Construct a map from a list of pairs, along with a list of keys with more
--   than one value (c.f., `Data.Map.fromList`, which discards duplicates).
--   The lists have disjoint keys - the 'normal' list contains only those keys
--   that don't have duplicate values.

fromListWithDups ∷ (Ord κ, Eq ν, Hashable ν, IsList l, Item l ~ (κ,ν)) ⇒
                    l → (Map κ (NonEmptyHashSet ν), Map κ ν)
fromListWithDups ls =
  let map = fromListWith (◇) (second pure ⊳ IsList.toList ls)
   in mapEither ( \ case (v :| []) → 𝕽 v; vs → 𝕷 (NonEmptyHashSet.fromList vs) )
                map

----------------------------------------

-- | like `fromListWithDups`, but instead of return both lists with & without
--   dups, this one returns the 'normal' list if there are no dups; else throws
--   the duplist

fromListDupsE ∷ (Ord κ, Eq ν, Hashable ν, IsList l, Item l ~ (κ,ν),
                 MonadError (MapDupKeyError κ ν) m, HasCallStack) ⇒
                 l → m (Map κ ν)
fromListDupsE ls =
  let (dups, res) = fromListWithDups ls
   in if null dups
      then return res
      else throwError $ MapDupKeyError dups callStack

----------------------------------------

-- | invert a map, that is, Map ν κ → Map κ ν; throw an error if duplicate
--   κ values are found

invertMap ∷ (Ord κ, Eq ν, Hashable ν, IsList l, Item l ~ (ν,κ),
             MonadError (MapDupKeyError κ ν) m) ⇒
            l → m (Map κ ν)
invertMap m = fromListDupsE (swap ⊳ IsList.toList m)

----------------------------------------

{- | invert a map which is a map to a list of values, that is,
     `Map ν [κ] → Map κ ν`; throw an error if duplicate κ values are found -}

invertMapS ∷ (Ord κ, Eq ν, Hashable ν, IsList l, Item l ~ (ν,[κ]),
              MonadError (MapDupKeyError κ ν) m) ⇒
             l → m (Map κ ν)
invertMapS m = fromListDupsE $ [ (v,k) | (k,vs) ← IsList.toList m, v ← vs ]

----------------------------------------

{- | merge a list of maps, throw if any key is duplicated -}
mergeMaps ∷ (Ord κ, Eq ν, Hashable ν, MonadError (MapDupKeyError κ ν) η,
             Foldable t, Functor t) ⇒
            t (Map κ ν) → η (Map κ ν)
mergeMaps maps = fromListDupsE (concat $ IsList.toList ⊳ maps)

-- that's all, folks! ---------------------------------------------------------
