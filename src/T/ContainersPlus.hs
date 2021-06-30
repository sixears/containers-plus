module T.ContainersPlus
  ( tests )
where

-- base --------------------------------

import System.Exit  ( ExitCode )
import System.IO    ( IO )

-- more-unicode ------------------------

import Data.MoreUnicode  ( 𝕊 )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus     ( runTestsP, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  ContainersPlus.T.Insert    as  Insert
import qualified  ContainersPlus.T.MapUtils  as  MapUtils

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "ContainersPlus" [ MapUtils.tests, Insert.tests ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

-- that's all, folks! ----------------------------------------------------------
