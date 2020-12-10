module Klank.SquareSketch where

import Prelude
import Control.Promise (toAffE)
import Data.Int (toNumber)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Typelevel.Num (D1)
import FRP.Behavior (Behavior)
import FRP.Behavior.Audio (AudioParameter, AudioUnit, decodeAudioDataFromUri, evalPiecewise, gain', gainT', playBuf, runInBrowser, speaker')
import Foreign.Object as O
import Type.Klank.Dev (Buffers, Klank, affable, defaultEngineInfo, klank)

kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

scene :: Number -> Behavior (AudioUnit D1)
scene time = pure $ speaker' (gainT' (epwf [ Tuple 0.0 0.0, Tuple 0.3 0.4, Tuple 0.6 0.9, Tuple 0.9 0.3, Tuple 3.0 0.0 ] time) (playBuf "scratch" 1.0))

buffers :: Buffers
buffers ctx _ =
  affable
    $ sequence
        ( O.singleton "scratch"
            $ toAffE (decodeAudioDataFromUri ctx "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/square1.ogg")
        )

main :: Klank
main =
  klank
    { run = runInBrowser scene
    , buffers = buffers
    }
