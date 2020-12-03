module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Klank.SilentNight (placeInSong)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Silent night" do
          it "should get measures right" do
            (show $ placeInSong 0.0) `shouldEqual` (show { measure: 0, beat: 0, eighth: 0, sixteenth: 0 })
            (show $ placeInSong 38.6) `shouldEqual` (show { beat: 1, eighth: 0, measure: 15, sixteenth: 1 })
            (show $ placeInSong 39.0) `shouldEqual` (show { beat: 1, eighth: 1, measure: 15, sixteenth: 3 })
