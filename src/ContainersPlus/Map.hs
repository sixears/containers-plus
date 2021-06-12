module ContainersPlus.Map
  ( fromList, __fromList )
where

import Prelude  ( (+) )

-- base --------------------------------

import Control.Monad  ( return )
import Data.Bool      ( Bool )
import Data.Either    ( either )
import Data.Eq        ( Eq )
import Data.Function  ( ($), flip, id )
import Data.List      ( repeat, zip )
import Data.Ord       ( Ord( (>) ) )
import Data.Tuple     ( fst )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- mono-traversable --------------------

import Data.Containers  ( ContainerKey, IsMap, MapValue, mapFromList )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Natural  ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- textual-plus ------------------------

import TextualPlus  ( __ERR__, q )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unordered-containers ----------------

import qualified Data.HashMap.Strict  as  HashMap
import Data.HashMap.Strict  ( HashMap )

--------------------------------------------------------------------------------

newtype RepeatedKeyError α = RepeatedKeyError [α]

instance Printable α ⇒ Printable (RepeatedKeyError α) where
  print (RepeatedKeyError ks) = P.text $ [fmt|repeated keys [%L]|] (q ⊳ ks)

------------------------------------------------------------

-- this was originally taken from Data.List.UniqueUnsorted, but that currently
-- doesn't build on NixOS
countMap :: (Hashable κ, Eq κ) ⇒ [κ] → HashMap κ ℕ
countMap = HashMap.fromListWith (+) ∘ flip zip (repeat 1)

repeatedBy ∷ (Hashable κ, Eq κ) ⇒ (ℕ → Bool) → [κ] → [κ]
repeatedBy p = HashMap.keys ∘ HashMap.filter p ∘ countMap

repeated ∷ (Hashable α, Eq α) ⇒ [α] → [α]
repeated = repeatedBy (>1)

{-| Like `HashMap.fromList`, but throws a RepeatedKeyError if any key is
     duplicated in the incoming list
 -}
fromList ∷ (Hashable κ, Eq κ, MonadError (RepeatedKeyError κ) η,
            ContainerKey σ ~ κ, MapValue σ ~ υ, IsMap σ) ⇒
           [(κ,υ)] → η σ
fromList kvs = case repeated $ fst ⊳ kvs of
                  []   → return $ mapFromList kvs
                  dups → throwError $ RepeatedKeyError dups

{-| `fromList`, but uses `error` in case of duplicate keys -}
__fromList ∷ (Hashable κ, Printable κ,
              ContainerKey σ ~ κ, MapValue σ ~ υ, IsMap σ) ⇒
             [(κ,υ)] → σ
__fromList = either __ERR__ id ∘ fromList

-- that's all, folks! ----------------------------------------------------------
