module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Klank.SilentNight (musicalInfoToTime, timeToMusicalInfo)
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
