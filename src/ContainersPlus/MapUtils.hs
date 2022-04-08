{- |

Description: utilities for working with Maps, with explicit (Monad)Errors
             when reading Lists with duplicate keys, etc.

-}

module ContainersPlus.MapUtils
  ( AsMapDupKeyError(..), MapDupKeyError( MapDupKeyError )
  , ToMapDupKeyError( toMapDupKeyError )
  , fromListWithDups, fromListDupsE, invertMap, invertMapS, mapFromList
  , mergeMaps, throwAsMapDupKeyError
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

{- | attempt was made to build a `Map` with one or more keys duplicated -}
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

{-| type that is convertable to a `MapDupKeyError` -}
class ToMapDupKeyError κ ν α where
  {-| convert to `MapDupKeyError` -}
  toMapDupKeyError ∷ HasCallStack ⇒ α → MapDupKeyError κ ν

--------------------

instance ToMapDupKeyError κ ν (MapDupKeyError κ ν) where
  toMapDupKeyError = id

--------------------

instance ToMapDupKeyError κ ν (Map κ (NonEmptyHashSet ν)) where
  toMapDupKeyError = \ xs → MapDupKeyError xs callStack

--------------------

instance (Ord κ, Hashable ν, Eq ν) ⇒ ToMapDupKeyError κ ν [(κ,ν)] where
  toMapDupKeyError = toMapDupKeyError ∘ mapFromList

------------------------------------------------------------

{-| prismatic type class for `MapDupKeyError` -}
class AsMapDupKeyError κ ν ε where
  {-| prism to `MapDupKeyError` -}
  _MapDupKeyError ∷ Prism' ε (MapDupKeyError κ ν)

instance AsMapDupKeyError κ ν (MapDupKeyError κ ν) where
  _MapDupKeyError = id

----------------------------------------

{-| throw a `MapDupKeyError` as an `AsMapDupKeyError` -}
throwAsMapDupKeyError ∷ (MonadError ε η, AsMapDupKeyError κ ν ε, HasCallStack) ⇒
                        Map κ (NonEmptyHashSet ν) → η α
throwAsMapDupKeyError m =
  throwError $ _MapDupKeyError # MapDupKeyError m callStack

------------------------------------------------------------

{-| convert a list of pairs to a map from keys to a set of values -}
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
   in mapEither (\ case (v :| []) → 𝕽 v; vs → 𝕷 (NonEmptyHashSet.fromList vs))
                map

----------------------------------------

-- | like `fromListWithDups`, but instead of return both lists with & without
--   dups, this one returns the 'normal' list if there are no dups; else throws
--   the duplist

fromListDupsE ∷ (Ord κ, Eq ν, Hashable ν, IsList l, Item l ~ (κ,ν),
                 AsMapDupKeyError κ ν ε, MonadError ε m, HasCallStack) ⇒
                 l → m (Map κ ν)
fromListDupsE ls =
  let (dups, res) = fromListWithDups ls
   in if null dups
      then return res
      else throwAsMapDupKeyError dups

----------------------------------------

-- | invert a map, that is, Map ν κ → Map κ ν; throw an error if duplicate
--   κ values are found

invertMap ∷ (Ord κ, Eq ν, Hashable ν, IsList l, Item l ~ (ν,κ),
             AsMapDupKeyError κ ν ε, MonadError ε m) ⇒
            l → m (Map κ ν)
invertMap m = fromListDupsE (swap ⊳ IsList.toList m)

----------------------------------------

{- | invert a map which is a map to a list of values, that is,
     `Map ν [κ] → Map κ ν`; throw an error if duplicate κ values are found -}

invertMapS ∷ (Ord κ, Eq ν, Hashable ν, IsList l, Item l ~ (ν,[κ]),
              AsMapDupKeyError κ ν ε, MonadError ε m) ⇒
             l → m (Map κ ν)
invertMapS m = fromListDupsE $ [ (v,k) | (k,vs) ← IsList.toList m, v ← vs ]

----------------------------------------

{- | merge a list of maps, throw if any key is duplicated -}
mergeMaps ∷ ∀ ε κ ν τ η .
            (Ord κ, Eq ν, Hashable ν, AsMapDupKeyError κ ν ε, MonadError ε η,
             Foldable τ, Functor τ) ⇒
            τ (Map κ ν) → η (Map κ ν)
mergeMaps maps = fromListDupsE (concat $ IsList.toList ⊳ maps)

-- that's all, folks! ---------------------------------------------------------
