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

-- more-unicode ------------------------

import Data.MoreUnicode.Bool  ( 𝔹 )

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

instance Eq α ⇒ HasMember [α] where
  type MemberType [α] = α
  member    = Data.List.elem
  notMember = Data.List.notElem

instance Ord α ⇒ HasMember (Set α) where
  type MemberType (Set α) = α
  member    = Data.Set.member
  notMember = Data.Set.notMember

instance Ord α ⇒ HasMember (Map α β) where
  type MemberType (Map α β) = α
  member    = Data.Map.member
  notMember = Data.Map.notMember

-- that's all, folks -----------------------------------------------------------
