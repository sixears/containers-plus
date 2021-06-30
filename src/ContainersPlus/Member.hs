-- {-# LANGUAGE UndecidableInstances #-}

{- | Functions about container membership. -}
module ContainersPlus.Member
  ( HasMember( MemberType, (∈), (∉), member, notMember ) )
where

-- base --------------------------------

import qualified Data.List

import Data.Bool      ( not )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Ord       ( Ord )

-- containers --------------------------

import qualified Data.Map
import qualified Data.Set

import Data.Map  ( Map )
import Data.Set  ( Set )

-- hashable ----------------------------

import Data.Hashable  ( Hashable )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool  ( 𝔹 )

-- unordered-containers ----------------

import qualified Data.HashMap.Strict
import qualified Data.HashSet

import Data.HashSet  ( HashSet )

--------------------------------------------------------------------------------

class HasMember α where
  type MemberType α
  member ∷  MemberType α → α → 𝔹
  (∈) ∷  MemberType α → α → 𝔹
  (∈) = member
  notMember ∷  MemberType α → α → 𝔹
  notMember x xs = not $ x ∈ xs
  (∉) ∷  MemberType α → α → 𝔹
  (∉) = notMember

--------------------

instance Eq α ⇒ HasMember [α] where
  type MemberType [α] = α
  member    = Data.List.elem
  notMember = Data.List.notElem

--------------------

instance Ord α ⇒ HasMember (Set α) where
  type MemberType (Set α) = α
  member    = Data.Set.member
  notMember = Data.Set.notMember

--------------------

instance Ord α ⇒ HasMember (Map α β) where
  type MemberType (Map α β) = α
  member    = Data.Map.member
  notMember = Data.Map.notMember

--------------------

instance (Eq α, Hashable α) ⇒ HasMember (HashSet α) where
  type MemberType (HashSet α) = α
  member    = Data.HashSet.member

--------------------

instance (Eq α, Hashable α) ⇒ HasMember (Data.HashMap.Strict.HashMap α β) where
  type MemberType (Data.HashMap.Strict.HashMap α β) = α
  member    = Data.HashMap.Strict.member

-- that's all, folks -----------------------------------------------------------
