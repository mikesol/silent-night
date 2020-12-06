module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Klank.SilentNight (miGap, musicalInfoToTime, roundUpTimeToNextMeasure, timeToMusicalInfo)
import Math (abs)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

epsilon = 0.00000001 :: Number

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Silent night" do
          it "should convert time to measures" do
            let
              p0 = timeToMusicalInfo 0.0
            p0.measure `shouldEqual` 0
            p0.beat `shouldEqual` 0.0
            let
              p1 = timeToMusicalInfo 38.6
            p1.measure `shouldEqual` 15
            (abs (p1.beat - 1.32) < epsilon) `shouldEqual` true
            let
              p2 = timeToMusicalInfo 39.0
            p2.measure `shouldEqual` 15
            (abs (p2.beat - 1.8) < epsilon) `shouldEqual` true
          it "should convert measures to time" do
            let
              p0 = musicalInfoToTime { measure: 15, beat: 1.32 }
            (abs (p0 - 38.6) < epsilon) `shouldEqual` true
          it "should produce a gap when necessary" do
            let
              g = miGap { measure: 0, beat: 1.3 } { measure: 3, beat: 0.0 }
            ((\x -> abs (x - 7.7) < epsilon) <$> g) `shouldEqual` Just true
          it "should produce a small gap when necessary" do
            let
              g = miGap { measure: 0, beat: 1.3 } { measure: 0, beat: 1.28 }
            ((\x -> abs (x + 0.02) < epsilon) <$> g) `shouldEqual` Just true
          it "should produce a very small gap when necessary" do
            let
              g = miGap { measure: 0, beat: 1.3 } { measure: 0, beat: 1.277 }
            ((\x -> abs (x + 0.023) < epsilon) <$> g) `shouldEqual` Just true
          it "should not produce a gap when out of range" do
            let
              g = miGap { measure: 0, beat: 1.3 } { measure: 0, beat: 1.276 }
            g `shouldEqual` Nothing
          it "should round up to next measure" do
            roundUpTimeToNextMeasure 2.0 `shouldEqual` { measure: 1, beat: 0.0 }
            roundUpTimeToNextMeasure 2.5 `shouldEqual` { measure: 1, beat: 0.0 }
            roundUpTimeToNextMeasure 2.51 `shouldEqual` { measure: 2, beat: 0.0 }
            roundUpTimeToNextMeasure 4.9 `shouldEqual` { measure: 2, beat: 0.0 }
            roundUpTimeToNextMeasure 5.0 `shouldEqual` { measure: 2, beat: 0.0 }
            roundUpTimeToNextMeasure 5.01 `shouldEqual` { measure: 3, beat: 0.0 }
