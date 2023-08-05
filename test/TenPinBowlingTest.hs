module TenPinBowlingTest
  ( tenPinTests,
  )
where

import TenPinBowling (Roll (Roll), Score (Score), score)
import Test.HUnit (Test (TestList), runTestTTAndExit)
import Test.HUnit.Base ((~:), (~?=))

tenPinTests :: IO ()
tenPinTests =
  runTestTTAndExit $
    TestList
      [ 
        test_score_noRolls_returnsNothing,
        test_score_allGutters_returnsZero,
        test_score_standardRolls_returnAddedScore,
        test_score_notEnoughRollsToFillFrames_returnsNothing,
        test_score_allSpares_addsBonuses,
        test_score_allStrikes_addsBonuses,
        test_score_oneSpare_addsBonus,
        test_score_oneStrike_addsBonus,
        test_score_extraRollsAreIgnored
      ]

gutter = Roll 0

test_score_noRolls_returnsNothing =
  "test_score_noRolls_returnsZero" ~:
    score [] ~?= Nothing

test_score_allGutters_returnsZero =
  "test_score_allGutters_returnsZero" ~:
    score rolls ~?= Just (Score 0)
  where
    rolls = replicate 20 gutter

test_score_standardRolls_returnAddedScore =
  "test_score_standardRolls_returnAddedScore" ~:
    score rolls ~?= Just (Score 80)
  where
    rolls = replicate 20 (Roll 4)


test_score_notEnoughRollsToFillFrames_returnsNothing =
  "test_score_notEnoughRollsToFillFrames_returnsNothing" ~:
    score rolls ~?= Nothing
  where
    rolls = replicate 19 (Roll 4)

test_score_oneSpare_addsBonus =
  "test_score_oneSpare_addsBonus" ~:
    score rolls ~?= Just (Score 18)
  where
    rolls = [Roll 3, Roll 7, Roll 4] ++ replicate 17 gutter

test_score_allSpares_addsBonuses =
  "test_score_allSpares_addsBonuses" ~:
    score rolls ~?= Just (Score 170)
  where
    rolls = concat (replicate 10 [Roll 7, Roll 3]) ++ [Roll 7]

test_score_oneStrike_addsBonus =
  "test_score_oneStrike_addsBonus" ~:
    score rolls ~?= Just (Score 24)
  where
    rolls = [Roll 10, Roll 3, Roll 4] ++ replicate 16 gutter

test_score_allStrikes_addsBonuses =
  "test_score_allStrikes_addsBonuses" ~:
    score rolls ~?= Just (Score 300)
  where
    rolls = replicate 12 (Roll 10)

test_score_extraRollsAreIgnored =
  "test_score_extraRollsAreIgnored" ~:
    score rolls ~?= Just (Score 150)
  where
    rolls = replicate 100 (Roll 5)
