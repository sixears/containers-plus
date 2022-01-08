{-| Tools for working with `Map` / `HashMap` -}
module ContainersPlus.Map
  ( AsRepeatedKeyError(..), RepeatedKeyError
  , fromList, __fromList, repeatedKeyError )
where

import Base1  hiding  ( fromList )

-- base --------------------------------

import Data.Function  ( flip )
import Data.List      ( repeat, zip )

-- mono-traversable --------------------

import Data.Containers  ( ContainerKey, IsMap, MapValue, mapFromList )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- textual-plus ------------------------

import TextualPlus  ( __ERR__, q )

-- unordered-containers ----------------

import qualified Data.HashMap.Strict  as  HashMap
import Data.HashMap.Strict  ( HashMap )

--------------------------------------------------------------------------------

{-| error caused by repeated keys used to, e.g., create a `Map` -}
data RepeatedKeyError α = RepeatedKeyError [α] CallStack
  deriving Show

----------

instance (Typeable α, Show α) ⇒ Exception (RepeatedKeyError α)

----------

instance Eq α ⇒ Eq (RepeatedKeyError α) where
  RepeatedKeyError a _ == RepeatedKeyError b _ = a ≡ b

----------

instance HasCallstack (RepeatedKeyError α) where
  callstack = lens (\ (RepeatedKeyError _ cs) → cs)
                   (\ (RepeatedKeyError xs _) cs → RepeatedKeyError xs cs)

----------

instance Printable α ⇒ Printable (RepeatedKeyError α) where
  print (RepeatedKeyError ks _) = P.text $ [fmt|repeated keys [%L]|] (q ⊳ ks)

----------

{-| create a `RepeatedKeyError` -}
repeatedKeyError ∷ HasCallStack ⇒ [α] → RepeatedKeyError α
repeatedKeyError xs = RepeatedKeyError xs callStack

----------------------------------------

{-| prismatic type class for `RepeatedKeyError` -}
class AsRepeatedKeyError α ε where
  _RepeatedKeyError ∷ Prism' ε (RepeatedKeyError α)

instance AsRepeatedKeyError α (RepeatedKeyError α) where
  _RepeatedKeyError = id

----------

asRepeatedKeyError ∷ (AsRepeatedKeyError α ε, HasCallStack) ⇒ [α] → ε
asRepeatedKeyError = (_RepeatedKeyError #) ∘ repeatedKeyError

throwAsRepeatedKeyError ∷ (AsRepeatedKeyError α ε,HasCallStack,MonadError ε η) ⇒
                          [α] → η ω
throwAsRepeatedKeyError = throwError ∘ asRepeatedKeyError

------------------------------------------------------------

-- this was originally taken from Data.List.UniqueUnsorted, but that currently
-- doesn't build on NixOS
countMap :: (Hashable κ, Eq κ) ⇒ [κ] → HashMap κ ℕ
countMap = HashMap.fromListWith (+) ∘ flip zip (repeat 1)

repeatedBy ∷ (Hashable κ, Eq κ) ⇒ (ℕ → 𝔹) → [κ] → [κ]
repeatedBy p = HashMap.keys ∘ HashMap.filter p ∘ countMap

repeated ∷ (Hashable α, Eq α) ⇒ [α] → [α]
repeated = repeatedBy (>1)

{-| Like `HashMap.fromList`, but throws a RepeatedKeyError if any key is
     duplicated in the incoming list
 -}
fromList ∷ ∀ ε σ κ υ η .
           (Hashable κ, Eq κ, AsRepeatedKeyError κ ε, MonadError ε η,
            ContainerKey σ ~ κ, MapValue σ ~ υ, IsMap σ) ⇒
           [(κ,υ)] → η σ
fromList kvs = case repeated $ fst ⊳ kvs of
                  []   → return $ mapFromList kvs
                  dups → throwAsRepeatedKeyError dups

_fromList ∷ ∀ σ κ υ η .
            (Hashable κ, Eq κ, MonadError (RepeatedKeyError κ) η,
            ContainerKey σ ~ κ, MapValue σ ~ υ, IsMap σ) ⇒ [(κ,υ)] → η σ
_fromList = fromList

{-| `fromList`, but uses `error` in case of duplicate keys -}
__fromList ∷ (Hashable κ, Printable κ,
              ContainerKey σ ~ κ, MapValue σ ~ υ, IsMap σ) ⇒
             [(κ,υ)] → σ
__fromList = either __ERR__ id ∘ _fromList

-- that's all, folks! ----------------------------------------------------------
