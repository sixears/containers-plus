{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module ContainersPlus.T.Insert
  ( _test, tests )
where

-- base --------------------------------

import Data.Function  ( ($) )
import Data.Int       ( Int )
import Data.String    ( String )
import GHC.Exts       ( fromList )
import System.Exit    ( ExitCode )
import System.IO      ( IO )

-- containers --------------------------

import Data.Map  ( Map )
import Data.Set  ( Set )

-- more-unicode ------------------------

import Data.MoreUnicode.Char    ( ℂ )
import Data.MoreUnicode.Monoid  ( ф )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus     ( runTestsP, runTestTree )

-- unordered-containers ----------------


import Data.HashMap.Strict  ( HashMap )
import Data.HashSet         ( HashSet )


------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ContainersPlus.Insert  ( (+>), (<+) )

-------------------------------------------------------------------------------

seven ∷ Int
seven = 7

setSeven ∷ Set Int
setSeven = fromList [7]

setSix ∷ Set Int
setSix = fromList [6]

setSixSeven ∷ Set Int
setSixSeven = fromList [6,7]

mapSeven ∷ Map ℂ Int
mapSeven = fromList [('d',7)]

mapSix ∷ Map ℂ Int
mapSix = fromList [('k',6)]

mapSixSeven ∷ Map ℂ Int
mapSixSeven = fromList [('k',6),('d',7)]

hashSetSeven ∷ HashSet Int
hashSetSeven = fromList [7]

hashSetSix ∷ HashSet Int
hashSetSix = fromList [6]

hashSetSixSeven ∷ HashSet Int
hashSetSixSeven = fromList [6,7]

hashMapSeven ∷ HashMap ℂ Int
hashMapSeven = fromList [('d',7)]

hashMapSix ∷ HashMap ℂ Int
hashMapSix = fromList [('k',6)]

hashMapSixSeven ∷ HashMap ℂ Int
hashMapSixSeven = fromList [('k',6),('d',7)]

----------------------------------------

listTests ∷ TestTree
listTests = testGroup "[a]" [ testCase "7 +> []" $ [7] @=? seven +> ф
                            , testCase "[] <+ 7" $ [7] @=? ф <+ seven
                            , testCase "7 +> [6]" $ [7,6] @=? seven +> [6]
                            , testCase "[6] <+ 7" $ [6,7] @=? [6] <+ seven
                            ]

----------------------------------------

setTests ∷ TestTree
setTests =
  testGroup "Set a"
             [ testCase "7 +> []" $ setSeven @=? seven +> ф
             , testCase "[] <+ 7" $ setSeven @=? ф <+ seven
             , testCase "7 +> [6]" $ setSixSeven @=? seven +> setSix
             , testCase "[6] <+ 7" $ setSixSeven @=? setSix <+ seven
             ]

----------------------------------------

mapTests ∷ TestTree
mapTests =
  testGroup "Map a"
             [ testCase "7 +> []" $ mapSeven @=? ('d',seven) +> ф
             , testCase "[] <+ 7" $ mapSeven @=? ф <+ ('d',seven)
             , testCase "7 +> [6]" $ mapSixSeven @=? ('d',seven) +> mapSix
             , testCase "[6] <+ 7" $ mapSixSeven @=? mapSix <+ ('d',seven)
             ]

----------------------------------------

hashSetTests ∷ TestTree
hashSetTests =
  testGroup "HashSet a"
             [ testCase "7 +> []" $ hashSetSeven @=? seven +> ф
             , testCase "[] <+ 7" $ hashSetSeven @=? ф <+ seven
             , testCase "7 +> [6]" $ hashSetSixSeven @=? seven +> hashSetSix
             , testCase "[6] <+ 7" $ hashSetSixSeven @=? hashSetSix <+ seven
             ]

----------------------------------------

hashMapTests ∷ TestTree
hashMapTests =
  testGroup "HashMap a"
             [ testCase "7 +> []" $ hashMapSeven @=? ('d',seven) +> ф
             , testCase "[] <+ 7" $ hashMapSeven @=? ф <+ ('d',seven)
             , testCase "7 +> [6]" $
                 hashMapSixSeven @=? ('d',seven) +> hashMapSix
             , testCase "[6] <+ 7" $
                 hashMapSixSeven @=? hashMapSix <+ ('d',seven)
             ]

------------------------------------------------------------

tests ∷ TestTree
tests =
  testGroup "Insert" [ listTests, setTests, mapTests
                     , hashSetTests, hashMapTests ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

-- that's all, folks! ---------------------------------------------------------
