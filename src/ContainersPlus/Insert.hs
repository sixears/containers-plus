{- | Containers that have an `insert` function. -}
module ContainersPlus.Insert
  ( HasInsert( InsertType, (<+), (+>), (⨭), (⨮), (&>), (<&), insert ) )
where

-- base --------------------------------

import Data.Eq        ( Eq )
import Data.Function  ( flip )
import Data.Ord       ( Ord )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- containers --------------------------

import qualified Data.Map
import qualified Data.Set

import Data.Map  ( Map )
import Data.Set  ( Set )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- lens --------------------------------

import Control.Lens.Setter ( ASetter )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊧) )

-- unordered-containers ----------------

import qualified Data.HashMap.Strict
import qualified Data.HashSet

import Data.HashSet  ( HashSet )

--------------------------------------------------------------------------------

class HasInsert α where
  type InsertType α
  insert ∷ InsertType α → α → α
  (+>) ∷ InsertType α → α → α
  (+>) = insert
  (<+) ∷ α → InsertType α → α
  (<+) = flip insert
  (&>) ∷ ASetter β γ α α → InsertType α → β → γ
  (&>) x v = (x ⊧ (v +>))
  (⨮) ∷ ASetter β γ α α → InsertType α → β → γ
  (⨮) = (&>)
  (<&) ∷ ASetter β γ α α → InsertType α → β → γ
  (<&) x v = (x ⊧ (<+ v))
  (⨭) ∷ ASetter β γ α α → InsertType α → β → γ
  (⨭) = (<&)

--------------------

instance HasInsert [α] where
  type InsertType [α] = α
  insert x xs = x : xs
  xs <+ x = xs ⊕ [x]

--------------------

instance Ord α ⇒ HasInsert (Set α) where
  type InsertType (Set α) = α
  insert = Data.Set.insert

--------------------

instance Ord α ⇒ HasInsert (Map α β) where
  type InsertType (Map α β) = (α,β)
  insert (a,b) = Data.Map.insert a b


--------------------

instance (Eq α, Hashable α) ⇒ HasInsert (HashSet α) where
  type InsertType (HashSet α) = α
  insert = Data.HashSet.insert

--------------------

instance (Eq α, Hashable α) ⇒ HasInsert (Data.HashMap.Strict.HashMap α β) where
  type InsertType (Data.HashMap.Strict.HashMap α β) = (α,β)
  insert (a,b) = Data.HashMap.Strict.insert a b



-- that's all, folks! ----------------------------------------------------------
