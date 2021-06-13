{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module ContainersPlus.T.MapUtils
  ( _test, tests )
where

-- base --------------------------------

import Data.Either         ( Either( Left ) )
import Data.Foldable       ( concat )
import Data.Function       ( (.), ($) )
import Data.Functor        ( (<$>) )
import Data.Int            ( Int )
import Data.List           ( sort, sortOn )
import Data.List.NonEmpty  ( NonEmpty( (:|) ) )
import Data.Ord            ( Ord )
import Data.String         ( String )
import Data.Tuple          ( snd )
import GHC.Exts            ( toList )
import System.Exit         ( ExitCode )
import System.IO           ( IO )

-- containers --------------------------

import qualified  Data.Map
import Data.Map  ( Map )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- non-empty-containers ----------------

import NonEmptyContainers.NonEmptyHashSet ( NonEmptyHashSet
                                          , fromList, singleton )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@?=), testCase )

-- tasty-plus --------------------------

import TastyPlus     ( assertListEqS, assertListEqRS, runTestsP, runTestTree )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ContainersPlus.MapUtils  ( MapDupKeyError( MapDupKeyError )
                                , fromListDupsE
                                , fromListWithDups, invertMap, invertMapS
                                , mapFromList, mergeMaps
                                )

-------------------------------------------------------------------------------

mapList :: Ord k => [(k, v)] -> Map k v
mapList = Data.Map.fromList

simpleList :: [(String, Int)]
simpleList =  [ ("zero", 0), ("one", 1), ("two", 2) ]

simpleList2 :: [(String, Int)]
simpleList2 =  [ ("three", 3), ("four", 4) ]

simpleList3 :: [(String, Int)]
simpleList3 =  [ ("three", 3), ("two", 4) ]

simpleListInverted :: [(Int, String)]
simpleListInverted = [ (0, "zero"), (1, "one"), (2, "two") ]

complexList :: [(String, Int)]
complexList =  [("none", 0), ("one", 1), ("zero", 0)]

dupsList :: [(String, Int)]
dupsList =  [("zero", 0), ("one", 1), ("zero", 2)]

dupsListInverted :: [(Int, String)]
dupsListInverted = [ (0, "zero"), (1, "one"), (2, "zero") ]

simpleListS :: [(String, [Int])]
simpleListS =  [ ("zero", [0]), ("one", [1]), ("two", [2]) ]

complexListS :: [(String, [Int])]
complexListS =  [ ("none", [0]), ("one", [1,1]), ("zero", [0]), ("three", []) ]

dupsListS :: [(String, [Int])]
dupsListS =  [("zero", [0]), ("one", [1]), ("zero", [2]), ("three", [])]

----------------------------------------

mapFromListTests :: TestTree
mapFromListTests =
  testGroup "mapFromList"
    [ testCase "dupsList" $
            mapFromList dupsList
        @?= Data.Map.fromList [ ("zero", fromList (0 :| [2]))
                              , ("one" , fromList (1 :| []))] ]

----------------------------------------

fromListWithDupsTests :: TestTree
fromListWithDupsTests =
  let sLMap :: Map String Int
      (sLDups, sLMap) = fromListWithDups simpleList
      cLMap :: Map String Int
      (cLDups, cLMap) = fromListWithDups complexList
      dLMap :: Map String Int
      (dLDups, dLMap) = fromListWithDups dupsList
   in testGroup "fromListWithDupsTests" $
        concat [ assertListEqS "sLDups" (toList sLDups)
                                        ([] :: [(String, NonEmptyHashSet Int)])
               , assertListEqS "sLList" (sortOn snd $ toList sLMap) simpleList
               , assertListEqS "cLDups" (toList cLDups)
                                        ([] :: [(String, NonEmptyHashSet Int)])
               , assertListEqS "cLList" (sortOn snd $ toList cLMap)
                                       (sortOn snd complexList)
               , assertListEqS "dLDups" (toList dLDups)
                                        [("zero", fromList (0 :| [2]))]
               , assertListEqS "dLList" (sortOn snd $ toList dLMap) [("one",1)]
               ]

----------------------------------------

fromListDupsETests :: TestTree
fromListDupsETests =
  let sLMap :: MonadError (MapDupKeyError String Int) m => m (Map String Int)
      sLMap = fromListDupsE simpleList
      cLMap :: MonadError (MapDupKeyError String Int) m => m (Map String Int)
      cLMap = fromListDupsE complexList
      dLDups :: MonadError (MapDupKeyError String Int) m => m (Map String Int)
      dLDups = fromListDupsE dupsList
   in testGroup "fromListDupsETests" $
        concat [ assertListEqRS "sLList" (sortOn snd . toList <$> sLMap)
                                         simpleList
               , assertListEqRS "cLList" (sortOn snd . toList <$> cLMap)
                                        (sortOn snd complexList)
               , [ testCase "dlList" $
                         dLDups
                     @?= Left (MapDupKeyError $
                                 mapList [("zero", fromList (0 :| [2]))])
                 ]
               ]

----------------------------------------

invertMapTests :: TestTree
invertMapTests =
  let sLMap :: MonadError (MapDupKeyError Int String) m => m (Map Int String)
      sLMap = invertMap simpleList
      cLDups :: MonadError (MapDupKeyError Int String) m => m (Map Int String)
      cLDups = invertMap complexList
      dLMap :: MonadError (MapDupKeyError Int String) m => m (Map Int String)
      dLMap = invertMap dupsList
   in testGroup "invertMapTests" $
        concat [ assertListEqRS "sLList" (sort . toList <$> sLMap) simpleListInverted
               , [ testCase "cLList" $
                         cLDups
                     @?= Left (MapDupKeyError $
                                 mapList [(0, fromList ("none" :| ["zero"]))])
                 ]
               , assertListEqRS "dLList" (sort . toList <$> dLMap)
                                        dupsListInverted
               ]

----------------------------------------

invertMapSTests :: TestTree
invertMapSTests =
  let sLSMap :: MonadError (MapDupKeyError Int String) m => m (Map Int String)
      sLSMap = invertMapS simpleListS
      cLSDups :: MonadError (MapDupKeyError Int String) m => m (Map Int String)
      cLSDups = invertMapS complexListS
      dLSMap :: MonadError (MapDupKeyError Int String) m => m (Map Int String)
      dLSMap = invertMapS dupsListS
   in testGroup "invertMapSTests" $
        concat [ assertListEqRS "sLSList" (sort . toList <$> sLSMap)
                                simpleListInverted
               , [ testCase "cLSList" $
                         cLSDups
                     @?= Left (MapDupKeyError $
                                 mapList [ (0, fromList ("none" :| ["zero"]))
                                         , (1, singleton "one")
                                         ])
                 ]
               , assertListEqRS "dLSList" (sort . toList <$> dLSMap)
                                dupsListInverted
               ]

----------------------------------------

mergeMapsTests :: TestTree
mergeMapsTests =
  let
      testThis name maps expect =
        assertListEqRS name
                       (sortOn snd . toList <$> mergeMaps (mapList <$> maps))
                       expect
   in testGroup "mergeMaps" $
        concat [ testThis "noDups" [ simpleList, simpleList2 ]
                                   [ ("zero", 0), ("one", 1), ("two", 2)
                                   , ("three", 3), ("four", 4) ]
               , [ testCase "dups" $
                         mergeMaps (mapList <$> [ simpleList, simpleList3 ])
                     @?= Left (MapDupKeyError
                                 (mapList [("two", fromList (2 :| [4]))]))
                 ]
               ]


------------------------------------------------------------

tests ∷ TestTree
tests =
  testGroup "MapUtils" [ mapFromListTests, fromListWithDupsTests
                       , fromListDupsETests, invertMapTests, invertMapSTests
                       , mergeMapsTests
                       ]

_test ∷ IO ExitCode
_test = runTestTree tests

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

-- that's all, folks! ---------------------------------------------------------
