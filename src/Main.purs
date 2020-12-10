module Klank.SilentNight where

import Prelude
import Color (Color, rgb, rgba)
import Control.Monad.Reader (Reader, ask, asks, runReader)
import Control.Parallel (parallel, sequential)
import Control.Promise (toAffE)
import Data.Array (catMaybes, drop, filter, fold, head, range, zip)
import Data.Array as A
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable, foldl, traverse_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (floor, toNumber)
import Data.Lens (Fold', Lens', Setter', Getter', _1, _2, _Just, over, preview, set)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe, maybe')
import Data.Maybe.First (First)
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.NonEmpty as NE
import Data.Profunctor (lcmap)
import Data.Profunctor.Choice (class Choice)
import Data.Profunctor.Strong (class Strong)
import Data.Set as DS
import Data.String (Pattern(..), indexOf)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Data.Typelevel.Num (D2, D3, D4, D6, D8, d0, d1, d2, d3, d4, d5, d6, d7)
import Data.Vec (Vec, empty, fill, (+>))
import Data.Vec as V
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, try)
import Effect.Exception (Error)
import Effect.Now (now)
import Effect.Random (random)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Audio (AV(..), AudioContext, AudioParameter, AudioUnit, BrowserAudioBuffer, CanvasInfo(..), decodeAudioDataFromUri, defaultExporter, defaultParam, evalPiecewise, gainT_, gainT_', gain_, gain_', highpass_, loopBuf_, makePeriodicWave, notch_, pannerMono_, periodicOsc_, playBufT_, playBuf_, runInBrowser_, sinOsc_, speaker')
import FRP.Event (Event, makeEvent, subscribe)
import Foreign.Object as O
import Graphics.Canvas (Rectangle)
import Graphics.Drawing (Drawing, Point, arc, circle, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, text)
import Graphics.Drawing.Font (FontOptions, bold, font, italic, sansSerif)
import Math (abs, cos, pi, pow, sin, (%))
import Prim.Row (class Cons)
import Type.Klank.Dev (Klank', affable, defaultEngineInfo, klank)
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Navigator (userAgent)
import Web.HTML.Window (navigator, toEventTarget)
import Web.TouchEvent.Touch as T
import Web.TouchEvent.TouchEvent (TouchEvent, changedTouches, fromEvent)
import Web.TouchEvent.TouchList as TL
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as ME

-- Square :: Middle motion when clicked, final is explosion outward
-- Triangle :: Change so that it is playing at 8th rhythm, mute
-- Gears :: Accelerate + different directions, rhythm is the one that guides beat
-- Rise :: Six different instruments rising, pin to pitch
-- Shrink :: Recalibration
-- Large :: Subtract from thick pad
-- Bells :: Keyboard with changing harmony
-- Snow :: Bells
-- Motion :: Wind
-- Heart :: Beating
tempo = 72.0 :: Number

measureDur = 180.0 / tempo :: Number

crotchet = 60.0 / tempo :: Number

quaver = 30.0 / tempo :: Number

semiquaver = 15.0 / tempo :: Number

preCodaInMeasures = 3.0 :: Number -- really two, but slower

introInMeasures = 4.0 :: Number

silentNightInMeasures = 26.0 :: Number -- includes 2m transition

silentNightInBeats = silentNightInMeasures * 3.0

silentNightInBeatsAsTime = silentNightInBeats * 60.0 / tempo

pieceInMeasures = measureDur * (silentNightInMeasures + silentNightInMeasures + silentNightInMeasures + introInMeasures + preCodaInMeasures) :: Number

roundUpTimeToNextMeasure :: Number -> MusicalInfo
roundUpTimeToNextMeasure t =
  let
    asM = t * tempo / (3.0 * 60.0)

    fl = floor asM
  in
    { measure: fl + if toNumber fl == asM then 0 else 1, beat: 0.0 }

type MusicalInfo
  = { measure :: Int
    , beat :: Number
    }

type VerseStarts
  = { one :: Maybe (Tuple Number VerseChoice) -- chosenAt choice
    , two :: Maybe (Tuple Number VerseChoice) -- chosenAt choice
    , three :: Maybe (Tuple Number VerseChoice) -- chosenAt choice
    }

type AudioEnv
  = { initiatedCoda :: Boolean
    , mainStarts :: Maybe Number
    , audioMarkers :: AudioMarkers
    , verseStarts :: VerseStarts
    , time :: Number
    , musicalInfo :: MusicalInfo
    }

type MusicM
  = Reader AudioEnv

type AudioListD2
  = List (AudioUnit D2)

pure2 :: forall m0 m1 a. Applicative m0 => Applicative m1 => a -> m0 (m1 a)
pure2 = pure <<< pure

metronomeClick :: Number -> Number -> MusicM AudioListD2
metronomeClick s gp = do
  let
    bo = beatGapToStartOffsetAsParam s gp
  pure2
    $ playBufT_ ("buffer" <> show s) "metronome-wb" bo

metronome :: MusicM AudioListD2
metronome = do
  { musicalInfo } <- ask
  let
    mmm1 = musicalInfo { measure = musicalInfo.measure `mod` 1 }
  let
    o
      | musicalInfo.measure == 0 && musicalInfo.beat < 0.5 = metronomeClick 0.9 0.0
      | Just gap <- startM1 |< mmm1 = metronomeClick 0.9 gap
      | mmm1 ||< startM0_1 = metronomeClick 0.9 0.0
      | Just gap <- startM0_1 |< mmm1
      , mmm1 ||< startM0_2 = metronomeClick 1.1 gap
      | Just gap <- startM0_2 |< mmm1
      , mmm1 ||< startM1 = metronomeClick 1.6 gap
      | otherwise = mempty
  o

data IntroLoop
  = IntroLoopA
  | IntroLoopB
  | IntroLoopC
  | IntroLoopD
  | IntroLoopE

derive instance eqIntroLoop :: Eq IntroLoop

il2s :: IntroLoop -> String
il2s IntroLoopA = "IntroLoopA"

il2s IntroLoopB = "IntroLoopB"

il2s IntroLoopC = "IntroLoopC"

il2s IntroLoopD = "IntroLoopD"

il2s IntroLoopE = "IntroLoopE"

startM0 = mmi 0 0.0 :: MusicalInfo

startM0_1 = mmi 0 1.0 :: MusicalInfo

startM0_2 = mmi 0 2.0 :: MusicalInfo

startM1 = mmi 1 0.0 :: MusicalInfo

startM4 = mmi 4 0.0 :: MusicalInfo

startM8 = mmi 8 0.0 :: MusicalInfo

startM12 = mmi 12 0.0 :: MusicalInfo

startM16 = mmi 16 0.0 :: MusicalInfo

startM20 = mmi 20 0.0 :: MusicalInfo

miGap :: MusicalInfo -> MusicalInfo -> Maybe Number
miGap target atNow =
  let
    targetB = toNumber target.measure * 3.0 + target.beat

    atNowB = toNumber atNow.measure * 3.0 + atNow.beat

    diffB = atNowB - targetB
  in
    if diffB <= nkrt then Nothing else Just diffB

miGapB :: MusicalInfo -> MusicalInfo -> Boolean
miGapB target now = isJust $ miGap target now

infixl 4 miGap as |<

infixl 4 miGapB as ||<

beatToTime :: Number -> Number
beatToTime b = (b * 60.0) / tempo

beatGapToStartOffset :: Number -> Number
beatGapToStartOffset n = max 0.0 $ kr - beatToTime n

beatGapToStartOffsetAsParam :: Number -> Number -> AudioParameter
beatGapToStartOffsetAsParam param n =
  defaultParam
    { param = param
    , timeOffset = beatGapToStartOffset n
    }

introLoopSingleton :: IntroLoop -> Number -> MusicM (AudioUnit D2)
introLoopSingleton il gp = let ils = il2s il in pure $ playBufT_ ("buf" <> ils) ils (beatGapToStartOffsetAsParam 1.0 gp)

introLoopPlayer :: MusicM AudioListD2
introLoopPlayer = do
  { musicalInfo, mainStarts, time } <- ask
  let
    mmm40 = musicalInfo { measure = musicalInfo.measure `mod` 20 }

    gbms = maybe Just (\t -> if t - time < kr then const Nothing else Just) mainStarts
  let
    o
      | musicalInfo ||< startM4 =
        sequence
          $ introLoopSingleton IntroLoopA 0.0
          : Nil
      | Just gap <- startM20 |< mmm40 =
        (sequence <<< L.catMaybes)
          $ gbms (introLoopSingleton IntroLoopA gap)
          : pure (introLoopSingleton IntroLoopE 0.0)
          : Nil
      | mmm40 ||< startM4 =
        (sequence <<< L.catMaybes)
          $ gbms (introLoopSingleton IntroLoopA 0.0)
          : pure (introLoopSingleton IntroLoopE 0.0)
          : Nil
      | Just gap <- startM4 |< mmm40
      , mmm40 ||< startM8 =
        (sequence <<< L.catMaybes)
          $ gbms (introLoopSingleton IntroLoopB gap)
          : pure (introLoopSingleton IntroLoopA 0.0)
          : Nil
      | Just gap <- startM8 |< mmm40
      , mmm40 ||< startM12 =
        (sequence <<< L.catMaybes)
          $ gbms (introLoopSingleton IntroLoopC gap)
          : pure (introLoopSingleton IntroLoopB 0.0)
          : Nil
      | Just gap <- startM12 |< mmm40
      , mmm40 ||< startM16 =
        (sequence <<< L.catMaybes)
          $ gbms (introLoopSingleton IntroLoopD gap)
          : pure (introLoopSingleton IntroLoopC 0.0)
          : Nil
      | Just gap <- startM16 |< mmm40 =
        (sequence <<< L.catMaybes)
          $ gbms (introLoopSingleton IntroLoopE gap)
          : pure (introLoopSingleton IntroLoopD 0.0)
          : Nil
      | otherwise = mempty
  o

introLoop :: MusicM AudioListD2
introLoop = introLoopPlayer

data ChoiceEvent
  = ChoiceEventA
  | ChoiceEventB
  | ChoiceEventC

derive instance choiceEventEq :: Eq ChoiceEvent

ding :: Number -> String -> PitchClass -> MusicM (AudioUnit D2)
ding n s p = do
  { time } <- ask
  pure (pannerMono_ ("panDing" <> s) 0.0 (gainT_' ("gainDing" <> s) (epwf [ Tuple 0.0 0.0, Tuple n 0.0, Tuple (n + 0.06) 0.2, Tuple (n + 0.3) 0.05, Tuple (n + 0.7) 0.0 ] time) (sinOsc_ ("sinDing" <> s) (conv440 (pcToRefMidi p)))))

choicePlayer :: String -> Number -> MusicM (AudioUnit D2)
choicePlayer tag pshift = pure (gain_' ("gainChoice" <> tag) 0.6 (playBuf_ ("bufChoice" <> tag) "choiceBell" pshift))

makeChoiceEvent :: ChoiceEvent -> (Maybe (Tuple Number VerseChoice)) -> MusicM AudioListD2
makeChoiceEvent _ Nothing = mempty

makeChoiceEvent ce (Just (Tuple st _)) = case ce of
  ChoiceEventA -> boundPlayer st 6.0 (pure <$> choicePlayer "ceA" 0.9)
  ChoiceEventB -> boundPlayer st 6.0 (pure <$> choicePlayer "ceB" 0.6723)
  ChoiceEventC -> boundPlayer st 6.0 (pure <$> choicePlayer "ceC" 1.01)

choiceEvents :: MusicM AudioListD2
choiceEvents = do
  { verseStarts: { one, two, three } } <- ask
  fold
    <$> sequence
        ( map (uncurry makeChoiceEvent)
            [ Tuple ChoiceEventA one
            , Tuple ChoiceEventB two
            , Tuple ChoiceEventC three
            ]
        )

introBG :: MusicM AudioListD2
introBG = do
  { time, mainStarts } <- ask
  if maybe false (\x -> time > x + 5.0) mainStarts then
    mempty
  else
    fold
      <$> sequence
          [ introLoop
          , choiceEvents
          ]

playVerse :: Number -> Verse -> VerseChoice -> MusicM AudioListD2
playVerse st v vc =
  let
    vvc = verseAndChoiceToString v vc
  in
    boundPlayer st (silentNightInBeatsAsTime + 5.0)
      ( do
          { time } <- ask
          let
            gp = st - time

            pm = if (gp < kr) then max gp 0.0 else 0.0
          pure2 $ playBufT_ ("verse_" <> vvc) vvc (defaultParam { param = 1.0, timeOffset = pm })
      )

verseToString :: Verse -> String
verseToString Verse1 = "1"

verseToString Verse2 = "2"

verseToString Verse3 = "3"

choiceToString :: VerseChoice -> String
choiceToString VersionOne = "1"

choiceToString VersionTwo = "2"

choiceToString VersionThree = "3"

choiceToString VersionFour = "4"

choiceToString VersionFive = "5"

choiceToString VersionSix = "6"

choiceToString VersionSeven = "7"

choiceToString VersionEight = "8"

verseAndChoiceToString :: Verse -> VerseChoice -> String
verseAndChoiceToString v vc = "v" <> verseToString v <> "t" <> choiceToString vc

verseThreeCorrective = 0.25 * crotchet :: Number

verseThreeStart = (2.0 * silentNightInBeatsAsTime) - ((5.0 * (3.0 * crotchet)) - verseThreeCorrective) :: Number

verses :: MusicM AudioListD2
verses = do
  { mainStarts, verseStarts: { one, two, three } } <- ask
  maybe mempty
    ( \mt ->
        fold
          <$> sequence
              ( catMaybes
                  [ (playVerse (mt - 3.0 * crotchet) Verse1 <<< snd) <$> one
                  , (playVerse (mt + silentNightInBeatsAsTime - 3.0 * crotchet) Verse2 <<< snd) <$> two
                  , (playVerse (mt + verseThreeStart) Verse3 <<< snd) <$> three
                  ]
              )
    )
    mainStarts

-- debug end flipping to start
-- market too long
-- birds too long
boundedEffect :: String -> (AudioEnv -> Maybe Number) -> (AudioEnv -> Maybe Number) -> (Number -> MusicM AudioListD2) -> MusicM AudioListD2
boundedEffect tag begt endt a = do
  audEnv <- ask
  let
    bt = begt audEnv
  maybe mempty
    ( \x -> case endt audEnv of
        Nothing ->  a x
        Just et
          | audEnv.time < et ->  a x
          | otherwise ->  mempty
    )
    bt

data TrianglePos
  = TriangleTop
  | TriangleLeft
  | TriangleRight

derive instance genericTrianglePos :: Generic TrianglePos _

instance showTrianglePos :: Show TrianglePos where
  show s = genericShow s

triangleSound :: TrianglePos -> Maybe Number -> Number -> MusicM (AudioUnit D2)
triangleSound tp dimT gp =
  if dimT == Nothing then
    mempty
  else
    let
      bo =
        beatGapToStartOffsetAsParam
          ( case tp of
              TriangleTop -> 2.0
              TriangleLeft -> 3.0
              TriangleRight -> 4.0
          )
          gp
    in
      pure $ playBufT_ ("buffer" <> show tp) "triangle" bo

triangle' :: Number -> MusicM AudioListD2
triangle' btm = do
  { musicalInfo } <- ask
  v <- asks getTriangleVector
  let
    mmm1 = musicalInfo { measure = musicalInfo.measure `mod` 1 }

    ffm = roundUpTimeToNextMeasure btm

    top = join $ flip V.index d0 <$> v

    left = join $ flip V.index d1 <$> v

    right = join $ flip V.index d2 <$> v
  let
    o
      | musicalInfo.measure == ffm.measure && musicalInfo.beat < 0.5 = sequence $ triangleSound TriangleTop top 0.0 : Nil
      | Just gap <- startM1 |< mmm1 =
        sequence
          $ triangleSound TriangleTop top gap
          : triangleSound TriangleRight right 0.0
          : Nil
      | mmm1 ||< startM0_1 =
        sequence
          $ triangleSound TriangleTop top 0.0
          : triangleSound TriangleRight right 0.0
          : Nil
      | Just gap <- startM0_1 |< mmm1
      , mmm1 ||< startM0_2 =
        sequence
          $ triangleSound TriangleLeft left gap
          : triangleSound TriangleTop top 0.0
          : Nil
      | Just gap <- startM0_2 |< mmm1
      , mmm1 ||< startM1 =
        sequence
          $ triangleSound TriangleRight right gap
          : triangleSound TriangleLeft left 0.0
          : Nil
      | otherwise = mempty
  o

data SquarePos
  = SquarePosTopLeft
  | SquarePosTopRight
  | SquarePosBottomLeft
  | SquarePosBottomRight

emptySoundListToBeFilled = mempty

squarePosToStr :: SquarePos -> String
squarePosToStr SquarePosTopLeft = "square1"

squarePosToStr SquarePosTopRight = "square2"

squarePosToStr SquarePosBottomLeft = "square3"

squarePosToStr SquarePosBottomRight = "square4"

squareSound :: SquarePos -> Number -> MusicM AudioListD2
squareSound tp st =
  let
    sps = squarePosToStr tp
  in
    do
      { time } <- ask
      pure2 (gainT_' ("gain_square_" <> sps) (epwf [ Tuple (st + 0.0) 0.0, Tuple (st + 0.3) 0.4, Tuple (st + 0.6) 0.9, Tuple (st + 0.9) 0.3, Tuple (st + 3.0) 0.0 ] time) (playBuf_ ("buf_square_" <> sps) sps 1.0))

squareP :: SquarePos -> Number -> MusicM AudioListD2
squareP sp t = boundPlayer t 5.0 (squareSound sp t)

squareEnd :: Number -> MusicM AudioListD2
squareEnd t =
  boundPlayer t 5.0
    ( do
        { time } <- ask
        bell <- choicePlayer "ceA" 1.01
        pure
          ( bell : (gainT_' ("gain_square_end") (epwf [ Tuple (t + 0.0) 0.0, Tuple (t + 0.3) 0.4, Tuple (t + 0.6) 0.9, Tuple (t + 0.9) 0.3, Tuple (t + 3.0) 0.0 ] time) (playBuf_ ("buf_square_end") "square5" 1.0)) : Nil
          )
    )

square' :: Number -> MusicM AudioListD2
square' btm = do
  { musicalInfo } <- ask
  v <- asks getSquareVector
  let
    topLeft = (squareP SquarePosTopLeft) <$> (join $ flip V.index d0 <$> v)

    topRight = (squareP SquarePosTopRight) <$> (join $ flip V.index d1 <$> v)

    bottomLeft = (squareP SquarePosBottomLeft) <$> (join $ flip V.index d2 <$> v)

    bottomRight = (squareP SquarePosBottomRight) <$> (join $ flip V.index d3 <$> v)

    endSound = (squareEnd <<< (_ + squareTravel) <<< foldl max 0.0) <$> (join (sequence <$> v))
  fold
    <$> sequence
        ( catMaybes
            [ topLeft, topRight, bottomLeft, bottomRight, endSound
            ]
        )

bindBetween :: Number -> Number -> Number -> Number
bindBetween mn mx n = max mn (min mx n)

bb01 :: Number -> Number
bb01 = bindBetween 0.0 1.0

maxMotionVelocity = 1.0 / 0.2 :: Number

minMotionVelocity = 0.0 :: Number

motion' :: Number -> MusicM AudioListD2
motion' st = do
  velocity <-
    ( case _ of
        Nothing -> 0.0
        Just (Tuple { x: x0, y: y0 } { x: x1, y: y1 }) -> (pythag (x1 - x0) (y1 - y0)) / kr
    )
      <$> asks getMotionPoints
  pure2 $ gain_' ("motionGain") (bindBetween 0.2 0.8 (calcSlope minMotionVelocity 0.2 maxMotionVelocity 0.8 velocity)) (playBuf_ ("motionBuffer") "motion" (bindBetween 1.0 1.15 (calcSlope minMotionVelocity 1.0 maxMotionVelocity 1.15 velocity)))

data GearPos
  = GearOne
  | GearTwo
  | GearThree
  | GearFour

gp2s :: GearPos -> String
gp2s GearOne = "gear1"

gp2s GearTwo = "gear2"

gp2s GearThree = "gear3"

gp2s GearFour = "gear4"

gp2r :: GearPos -> Number
gp2r GearOne = 0.9

gp2r GearTwo = 1.0

gp2r GearThree = 1.3

gp2r GearFour = 1.6

gp2pr :: GearPos -> Number
gp2pr GearOne = 0.2

gp2pr GearTwo = 0.3

gp2pr GearThree = 0.4

gp2pr GearFour = 0.5

gearMaxVol = 0.7 :: Number

gearSound :: Maybe Number -> GearPos -> Number -> MusicM AudioListD2
gearSound fadeOutT pos startT =
  let
    gn = gp2s pos
  in
    do
      { time } <- ask
      pure2 (pannerMono_ ("gainPM" <> gn) (sin $ (time - startT) * pi * gp2pr pos) (gain_' ("gain" <> gn) (maybe gearMaxVol (\fot -> bindBetween 0.0 gearMaxVol (calcSlope fot gearMaxVol (fot + 4.0) 0.0 time)) fadeOutT) (loopBuf_ ("buffer_" <> gn) "gearBowl" (gp2r pos) 4.0 10.0)))

gearsP :: Maybe Number -> GearPos -> Number -> MusicM AudioListD2
gearsP fadeOutT pos startT = boundPlayer startT 10000.0 (gearSound fadeOutT pos startT)

gears' :: Number -> MusicM AudioListD2
gears' btm = do
  { musicalInfo } <- ask
  v <- asks getGearsVector
  let
    fadeOutStarts = (foldl max 0.0) <$> (join (sequence <$> v))

    gp = gearsP fadeOutStarts

    one = (gp GearOne) <$> (join $ flip V.index d0 <$> v)

    two = (gp GearTwo) <$> (join $ flip V.index d1 <$> v)

    three = (gp GearThree) <$> (join $ flip V.index d2 <$> v)

    four = (gp GearFour) <$> (join $ flip V.index d3 <$> v)
  fold
    <$> sequence
        ( catMaybes
            [ one, two, three, four ]
        )

data LargeTrack
  = LgSanta
  | LgBirds
  | LgChimes
  | LgSynth
  | LgCrowd

lt2s :: LargeTrack -> String
lt2s LgBirds = "large-birds"

lt2s LgCrowd = "large-market"

lt2s LgChimes = "large-chimes"

lt2s LgSynth = "large-synth"

lt2s LgSanta = "large-santa"

lg2f :: Int -> LargeTrack -> AudioUnit D2 -> AudioUnit D2
lg2f i LgCrowd
  | i == 0 = identity
  | i == 1 = (notch_ "lg-notch-1" 300.0 1.0)
  | i == 2 = (notch_ "lg-notch-2" 900.0 1.0) <<< lg2f 1 LgCrowd
  | i == 3 = (notch_ "lg-notch-3" 1400.0 1.0) <<< lg2f 2 LgCrowd
  | i == 4 = (notch_ "lg-notch-4" 2000.0 1.0) <<< lg2f 3 LgCrowd
  | otherwise = (notch_ "lg-notch-5" 2500.0 1.0) <<< lg2f 4 LgCrowd

lg2f _ _ = identity

largeSingleton :: Int -> Number -> LargeTrack -> Maybe Number -> MusicM AudioListD2
largeSingleton nfilt st ltrack ed =
  let
    ln = lt2s ltrack

    ft = lg2f nfilt ltrack

    ept = st + largeCrossing
  in
    do
      { time } <- ask
      pure2 (ft (gain_' ("gain_large_" <> ln) (1.0 * (bb01 $ calcSlope (ept - 5.0) 1.0 ept 0.0 time) * (bb01 $ calcSlope st 0.0 (st + 4.0) 1.0 time) * (maybe 1.0 (\ee -> bb01 $ calcSlope ee 1.0 (ee + 4.0) 0.0 time) ed)) (loopBuf_ ("loop_large_" <> ln) ln 1.0 0.0 0.0)))

makeLarges :: Int -> Number -> Maybe Number -> Maybe Number -> Maybe Number -> Maybe Number -> MusicM AudioListD2
makeLarges nfilt st birdsOut santaOut chimesOut synthOut =
  let
    lfunc = largeSingleton nfilt st
  in
    fold
      <$> sequence [ lfunc LgSanta santaOut, lfunc LgBirds birdsOut, lfunc LgChimes chimesOut, lfunc LgSynth synthOut, lfunc LgCrowd Nothing ]

--  large start
largeListF :: Number -> List Number -> MusicM AudioListD2
largeListF st Nil = makeLarges 0 st Nothing Nothing Nothing Nothing

largeListF st (a : Nil) = makeLarges 0 st (Just a) Nothing Nothing Nothing

largeListF st (b : a : Nil) = makeLarges 0 st (Just a) (Just b) Nothing Nothing

largeListF st (c : b : a : Nil) = makeLarges 0 st (Just a) (Just b) (Just c) Nothing

largeListF st (d : c : b : a : Nil) = makeLarges 0 st (Just a) (Just b) (Just c) (Just d)

largeListF st (e : d : c : b : a : Nil) = makeLarges 1 st (Just a) (Just b) (Just c) (Just d)

largeListF st (f : e : d : c : b : a : Nil) = makeLarges 2 st (Just a) (Just b) (Just c) (Just d)

largeListF st (g : f : e : d : c : b : a : Nil) = makeLarges 3 st (Just a) (Just b) (Just c) (Just d)

largeListF st (h : g : f : e : d : c : b : a : Nil) = makeLarges 4 st (Just a) (Just b) (Just c) (Just d)

-- ignore anything larger than 7 filters
-- even this is probably too big...
largeListF st (foo : bar) = largeListF st bar

large' :: Number -> MusicM AudioListD2
large' begT = do
  { musicalInfo } <- ask
  ll <- fromMaybe Nil <$> asks getLargeList
  largeListF begT ll

snowSingleton :: Int -> Number -> MusicM AudioListD2
snowSingleton i st = boundPlayer st 4.0 (pure2 (gain_' ("snowGain_" <> show i) (fromMaybe 0.3 (A.index [ 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8 ] (i `mod` 7))) (playBuf_ ("snowBuf_" <> show i) "snow" (conv1 (fromMaybe 1.0 $ A.index [ 1.0, 6.0, 8.0, 10.0, 13.0, 18.0, 20.0, 22.0 ] (i `mod` 8))))))

snowAudio :: List (Maybe Number) -> MusicM AudioListD2
snowAudio l = fold <$> sequence (go 0 l)
  where
  go :: Int -> List (Maybe Number) -> List (MusicM AudioListD2)
  go i Nil = Nil

  go i (Nothing : b) = go (i + 1) b

  go i (Just a : b) = snowSingleton i a : go (i + 1) b

snow' :: Number -> MusicM AudioListD2
snow' begT = (fromMaybe baseSnows <$> asks getSnowList) >>= snowAudio

safeSustainTo8Wide :: NonEmpty List PitchClass -> Vec D8 (Tuple Int PitchClass)
safeSustainTo8Wide nel = atInL 0 +> atInL 1 +> atInL 2 +> atInL 3 +> atInL 4 +> atInL 5 +> atInL 6 +> atInL 7 +> empty
  where
  dflt = NE.head nel

  asL = L.fromFoldable (DS.fromFoldable (dflt : NE.tail nel))

  ll = L.length asL

  atInL i = Tuple (i `div` ll) (fromMaybe dflt (L.index asL $ i `mod` ll))

data BellInstrument
  = BellInstrument0
  | BellInstrument1
  | BellInstrument2
  | BellInstrument3

i2bi :: Int -> BellInstrument
i2bi 0 = BellInstrument0

i2bi 1 = BellInstrument1

i2bi 2 = BellInstrument2

i2bi 3 = BellInstrument3

i2bi _ = BellInstrument0

i2tip :: Int -> Vec D8 (Tuple Int PitchClass) -> Tuple Int PitchClass
i2tip 0 v = V.index v d0

i2tip 1 v = V.index v d1

i2tip 2 v = V.index v d2

i2tip 3 v = V.index v d3

i2tip 4 v = V.index v d4

i2tip 5 v = V.index v d5

i2tip 6 v = V.index v d6

i2tip 7 v = V.index v d7

i2tip _ v = V.index v d0

bellName :: BellInstrument -> Int -> PitchClass -> String
bellName bi oct pc =
  ( case bi of
      BellInstrument0 -> "m"
      BellInstrument1 -> "kg"
      BellInstrument2 -> "rb"
      BellInstrument3 -> "sb"
  )
    <> show
        ( ( case oct of
              0 -> 0
              1 -> 1
              2 -> 2
              3 -> 3
              _ -> 3
          )
            * pc2i pc
        )

singleBell' :: Number -> BellInstrument -> Tuple Int PitchClass -> MusicM AudioListD2
singleBell' onset bi (Tuple oct pc) = let bn = bellName bi oct pc in pure2 (playBuf_ (bn <> show onset) bn 1.0)

singleBell :: Vec D8 (Tuple Int PitchClass) -> Int -> List Number -> List (MusicM AudioListD2)
singleBell v i a = map (\inc -> boundPlayer inc 2.0 (singleBell' inc (i2bi (i `div` 8)) (i2tip (i `mod` 8) v))) a

bellAudio :: Vec D8 (Tuple Int PitchClass) -> List (List Number) -> MusicM AudioListD2
bellAudio v l = fold <$> sequence (join $ go 0 l)
  where
  go :: Int -> List (List Number) -> List (List (MusicM AudioListD2))
  go i Nil = mempty

  go i (a : b) = singleBell v i a : go i b

bells' :: Number -> MusicM AudioListD2
bells' begT = do
  { time, mainStarts } <- ask
  case mainStarts of
    Nothing -> mempty
    Just ms -> do
      bellsL <- fromMaybe baseBells <$> asks getBellsList
      let
        placeInPiece = timeToMusicalInfo (time - ms)

        twoBeatsAhead
          | placeInPiece.beat >= 1.0 = { measure: placeInPiece.measure + 1, beat: (placeInPiece.beat + 2.0) % 3.0 }
          | otherwise = { measure: placeInPiece.measure, beat: placeInPiece.beat + 2.0 }

        ss = safeSustainTo8Wide (safeSustain' placeInPiece twoBeatsAhead)
      bellAudio ss bellsL

data ShrinkPos
  = ShrinkOne
  | ShrinkTwo
  | ShrinkThree
  | ShrinkFour
  | ShrinkFive
  | ShrinkSix

derive instance genericShrinkPos :: Generic ShrinkPos _

instance showShrinkPos :: Show ShrinkPos where
  show s = genericShow s

shrinkL = standardIntro + shrinkNormal + standardOutro :: Number

shrinkP :: ShrinkPos -> Number -> Number -> MusicM AudioListD2
shrinkP rp startT currentShrinkPlace = do
  { time } <- ask
  pure2
    $ wah (show rp) "smooth" shrinkL
        ( case rp of
            ShrinkOne -> 100
            ShrinkTwo -> 110
            ShrinkThree -> 120
            ShrinkFour -> 130
            ShrinkFive -> 140
            ShrinkSix -> 150
        )
        ( pure
            $ case rp of
                ShrinkOne -> 75.0 -- Eb Bb C Eb F Bb C
                ShrinkTwo -> 82.0
                ShrinkThree -> 84.0
                ShrinkFour -> 87.0
                ShrinkFive -> 89.0
                ShrinkSix -> 91.0
        )
        (\t -> 0.2 * bb01 (calcSlope startT 0.0 (startT + standardIntro) 1.0 t) * bb01 (calcSlope (startT + standardIntro + shrinkNormal) 1.0 (startT + standardIntro + shrinkNormal + standardOutro) 0.0 t) * currentShrinkPlace)
        ( \t ->
            sin
              $ ( t
                    + case rp of
                        ShrinkOne -> 0.2
                        ShrinkTwo -> 0.4
                        ShrinkThree -> 0.5
                        ShrinkFour -> 0.6
                        ShrinkFive -> 0.8
                        ShrinkSix -> 0.9
                )
              * pi
              * ( case rp of
                    ShrinkOne -> 0.2
                    ShrinkTwo -> 0.1
                    ShrinkThree -> 0.3
                    ShrinkFour -> 0.4
                    ShrinkFive -> 0.5
                    ShrinkSix -> 0.6
                )
        )
        time

shrink' :: Number -> MusicM AudioListD2
shrink' btm = do
  { musicalInfo } <- ask
  v' <- asks getShrinkVector
  case v' of
    Nothing -> mempty
    Just v -> do
      let
        one = shrinkP ShrinkOne btm (V.index v d0)

        two = shrinkP ShrinkTwo btm (V.index v d1)

        three = shrinkP ShrinkThree btm (V.index v d2)

        four = shrinkP ShrinkFour btm (V.index v d3)

        five = shrinkP ShrinkFive btm (V.index v d4)

        six = shrinkP ShrinkSix btm (V.index v d5)
      fold
        <$> sequence
            [ one, two, three, four, five, six ]

riseMaxVol = 1.0 :: Number

riseF :: Number -> Maybe Number -> Number -> Number -> Number
riseF startT didStop time gn = gn * (maybe riseMaxVol (\n -> calcSlope n (riseMaxVol) (n + 2.0) 0.5 time) didStop)

rise' :: Number -> MusicM AudioListD2
rise' btm = do
  { musicalInfo, time } <- ask
  v <- asks getRiseVector
  let
    one = riseF btm (join $ flip V.index d0 <$> v) time

    two = riseF btm (join $ flip V.index d1 <$> v) time

    three = riseF btm (join $ flip V.index d2 <$> v) time

    four = riseF btm (join $ flip V.index d3 <$> v) time

    five = riseF btm (join $ flip V.index d4 <$> v) time

    six = riseF btm (join $ flip V.index d5 <$> v) time

    riseEvl = (one <<< two <<< three <<< four <<< five <<< six) 1.0
  pure2 (highpass_ "riseHPF" 1900.0 3.0 (gain_' ("riseGain") riseEvl (playBuf_ "riseBuf" "rise" (if time < standardIntro + btm then 1.0 else (1.0 - riseEvl) + (calcSlope (btm + standardIntro) 1.0 (btm + standardIntro + riseNormal) 1.7 time)))))

heart' :: Number -> MusicM AudioListD2
heart' btm = do
  { musicalInfo } <- ask
  v <- asks getHeartStartTime
  case v of
    Nothing -> mempty
    Just t ->
      boundPlayer t (heartNormal + standardOutro)
        ( fold
            <$> sequence
                ( join
                    ( map
                        ( \i ->
                            [ boundPlayer (t + 0.2 + (toNumber i) * 1.45) 2.0 (pure2 $ playBuf_ ("hb1" <> show i) "hb1" 1.0), boundPlayer (t + 0.7 + (toNumber i) * 1.45) 2.0 (pure2 $ playBuf_ ("hb2" <> show i) "hb2" 1.0)
                            ]
                        )
                        (range 0 30)
                    )
                )
        )

triangle :: MusicM AudioListD2
triangle = boundedEffect "triangle" getTriangleBegTime getTriangleEndTime triangle'

square :: MusicM AudioListD2
square = boundedEffect "square" getSquareBegTime getSquareEndTime square'

motion :: MusicM AudioListD2
motion = boundedEffect "motion" getMotionBegTime getMotionEndTime motion'

snow :: MusicM AudioListD2
snow = boundedEffect "snow" getSnowBegTime getSnowEndTime snow'

large :: MusicM AudioListD2
large = boundedEffect "large" getLargeBegTime getLargeEndTime large'

bells :: MusicM AudioListD2
bells = boundedEffect "bells" getBellsBegTime getBellsEndTime bells'

heart :: MusicM AudioListD2
heart = boundedEffect "heart" getHeartBegTime getHeartEndTime heart'

shrink :: MusicM AudioListD2
shrink = boundedEffect "shrink" getShrinkBegTime getShrinkEndTime shrink'

rise :: MusicM AudioListD2
rise = boundedEffect "rise" getRiseBegTime getRiseEndTime rise'

gears :: MusicM AudioListD2
gears = boundedEffect "gears" getGearsBegTime getGearsEndTime gears'

silentNight :: MusicM AudioListD2
silentNight =
  fold
    <$> sequence
        [ introBG
        , verses
        , triangle
        , square
        , motion
        , snow
        , gears
        , rise
        , large
        , shrink
        , bells
        , heart
        -- , metronome
        ]

mmi :: Int -> Number -> MusicalInfo
mmi measure beat = { measure, beat }

musicalInfoToTime :: MusicalInfo -> Number
musicalInfoToTime { measure, beat } = (toNumber measure * 3.0 + beat) * 60.0 / tempo

timeToMusicalInfo :: Number -> MusicalInfo
timeToMusicalInfo t =
  let
    beats = tempo * t / 60.0

    measure = floor (beats / 3.0)

    beat = beats - ((toNumber measure) * 3.0)
  in
    { measure, beat }

data PitchClass
  = Ab
  | A
  | Bb
  | Cb
  | C
  | Db
  | D
  | Eb
  | Fb
  | F
  | Gb
  | G

pc2i :: PitchClass -> Int
pc2i Ab = 0

pc2i A = 1

pc2i Bb = 2

pc2i Cb = 3

pc2i C = 4

pc2i Db = 5

pc2i D = 6

pc2i Eb = 7

pc2i Fb = 8

pc2i F = 9

pc2i Gb = 10

pc2i G = 11

derive instance eqPitchClass :: Eq PitchClass

instance ordPitchClass :: Ord PitchClass where
  compare a b = compare (pc2i a) (pc2i b)

codaStartsMI = { measure: 82, beat: 0.0 } :: MusicalInfo

codaStarts = musicalInfoToTime codaStartsMI :: Number

measureMinus :: MusicalInfo -> Int -> MusicalInfo
measureMinus { measure, beat } i = { measure: measure - i, beat }

introSafeSustainAb :: MusicalInfo -> NonEmpty List PitchClass
introSafeSustainAb ma
  | ma.beat < 2.0 = Ab :| C : Eb : Nil
  | ma.beat < 2.0 = Ab :| Ab : C : Eb : Eb : Nil
  | otherwise = Ab :| Eb : Nil

introSafeSustainDb :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
introSafeSustainDb ma mb
  | ma.measure == mb.measure = Ab :| Ab : Bb : Db : Db : F : Nil
  | otherwise = Ab :| Ab : Ab : Bb : Nil

introSafeSustain :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
introSafeSustain ma mb
  | ma.measure == 0 || ma.measure == 2 = introSafeSustainAb ma
  | otherwise = introSafeSustainDb ma mb

endSafeSustain :: Number -> NonEmpty List PitchClass
endSafeSustain a
  | a < 1.5 = Ab :| C : Eb : Eb : Nil
  | a < 3.0 = Ab :| Eb : Nil
  | a < 7.0 = Eb :| G : Bb : Nil
  | otherwise = Ab :| Ab : Bb : C : C : Eb : Eb : F : G : Nil

middleSafeSustain_0_4 :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain_0_4 ma mb
  | ma.measure > 2 && mb.measure < 4 = Ab :| Bb : C : Eb : G : Nil
  | mb.measure < 4 = Ab :| C : Eb : G : Nil
  | ma.measure > 2 && mb.measure < 8 = Bb :| Eb : Eb : G : Nil
  | otherwise = Ab :| Nil

middleSafeSustain_4_6 :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain_4_6 ma mb
  | ma.measure == 5 && mb.measure < 6 = Eb :| G : Bb : Db : Nil
  | mb.measure < 6 = Eb :| G : Bb : Nil
  | mb.measure < 8 = Eb :| Eb : G : Bb : Nil
  | otherwise = Eb :| Nil

middleSafeSustain_6_8 :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain_6_8 ma mb
  | mb.measure < 8 = Ab :| Ab : Bb : C : C : Eb : G : Nil
  | otherwise = Ab :| Eb : Nil

middleSafeSustain_8_10 :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain_8_10 ma mb
  | mb.measure < 10 = Db :| Db : F : F : Ab : Ab : Bb : Nil
  | otherwise = Ab :| Ab : Bb : Nil

middleSafeSustain_10_12 :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain_10_12 ma mb
  | mb.measure < 12 = Ab :| Ab : Bb : C : C : Eb : G : Nil
  | otherwise = Ab :| Eb : Nil

middleSafeSustain_12_14 :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain_12_14 ma mb
  | mb.measure < 14 = Db :| Db : F : F : Ab : Ab : Bb : Nil
  | otherwise = Ab :| Ab : Bb : Nil

middleSafeSustain_14_16 :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain_14_16 ma mb
  | mb.measure < 16 = Ab :| Ab : Bb : C : C : Eb : G : Nil
  | otherwise = Ab :| Eb : Nil

middleSafeSustain_16_18 :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain_16_18 ma mb
  | mb.measure < 19 = Eb :| G : Bb : Nil
  | otherwise = Eb :| Bb : Nil

middleSafeSustain_18_19 :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain_18_19 ma mb
  | mb.measure < 19 = Ab :| Ab : C : C : Eb : Eb : G : Nil
  | otherwise = Ab :| Eb : Nil

middleSafeSustain_19_20 :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain_19_20 ma mb
  | mb.measure < 20 = Db :| F : Ab : C : Nil
  | otherwise = Ab :| Ab : C : Nil

middleSafeSustain_20_21 :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain_20_21 ma mb
  | mb.measure < 21 = Ab :| C : Eb : Nil
  | otherwise = Ab :| Eb : Nil

middleSafeSustain_21_22 :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain_21_22 ma mb = Eb :| G : Bb : Nil

middleSafeSustain_22_23 = const introSafeSustainAb :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass

middleSafeSustain_23_24 = introSafeSustainDb :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass

middleSafeSustain_24_25 = const introSafeSustainAb :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass

middleSafeSustain_25_26 = introSafeSustainDb :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass

middleSafeSustain :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
middleSafeSustain ma mb
  | ma.measure < 4 = middleSafeSustain_0_4 ma mb
  | ma.measure < 6 = middleSafeSustain_4_6 ma mb
  | ma.measure < 8 = middleSafeSustain_6_8 ma mb
  | ma.measure < 10 = middleSafeSustain_8_10 ma mb
  | ma.measure < 12 = middleSafeSustain_10_12 ma mb
  | ma.measure < 14 = middleSafeSustain_12_14 ma mb
  | ma.measure < 16 = middleSafeSustain_14_16 ma mb
  | ma.measure < 18 = middleSafeSustain_16_18 ma mb
  | ma.measure < 19 = middleSafeSustain_18_19 ma mb
  | ma.measure < 20 = middleSafeSustain_19_20 ma mb
  | ma.measure < 21 = middleSafeSustain_20_21 ma mb
  | ma.measure < 22 = middleSafeSustain_21_22 ma mb
  | ma.measure < 23 = middleSafeSustain_22_23 ma mb
  | ma.measure < 24 = middleSafeSustain_23_24 ma mb
  | ma.measure < 25 = middleSafeSustain_24_25 ma mb
  | ma.measure < 26 = middleSafeSustain_25_26 ma mb
  | otherwise = Ab :| Nil

safeSustain' :: MusicalInfo -> MusicalInfo -> NonEmpty List PitchClass
safeSustain' ma mb =
  let
    sus
      | ma.measure < 4 = introSafeSustain ma mb
      | ma.measure >= 82 = endSafeSustain ((musicalInfoToTime ma) - codaStarts)
      | otherwise = middleSafeSustain (measureMinus (measureMinus ma $ 26 * (ma.measure `div` 26)) 4) (measureMinus (measureMinus mb $ 26 * (ma.measure `div` 26)) 4)
  in
    sus

safeSustain :: Number -> Number -> NonEmpty List PitchClass
safeSustain a b =
  let
    ma = timeToMusicalInfo a

    mb = timeToMusicalInfo b
  in
    safeSustain' ma mb

------
pcToRefMidi :: PitchClass -> Number
pcToRefMidi Ab = 56.0

pcToRefMidi A = 57.0

pcToRefMidi Bb = 58.0

pcToRefMidi Cb = 59.0

pcToRefMidi C = 60.0

pcToRefMidi Db = 61.0

pcToRefMidi D = 62.0

pcToRefMidi Eb = 63.0

pcToRefMidi Fb = 64.0

pcToRefMidi F = 65.0

pcToRefMidi Gb = 66.0

pcToRefMidi G = 67.0

conv440 :: Number -> Number
conv440 i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

pythag :: Number -> Number -> Number
pythag x y = ((x `pow` 2.0) + (y `pow` 2.0)) `pow` 0.5

conv1 :: Number -> Number
conv1 i = 1.0 * (2.0 `pow` ((i - 0.0) / 12.0))

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

loopT :: forall a. Number -> (Number -> a) -> (Number -> a)
loopT t = lcmap (_ % t)

foldOverTime :: forall a b f. Foldable f => Applicative f => Monoid (f b) => (Number -> a -> b) -> (a -> Number) -> f a -> f b
foldOverTime trans getn = _.acc <<< foldl (\{ acc, clen } a -> { acc: acc <> (pure $ trans clen a), clen: clen + getn a }) { acc: mempty, clen: 0.0 }

boundPlayer :: Number -> Number -> MusicM AudioListD2 -> MusicM AudioListD2
boundPlayer st len a = do
  { time } <- ask
  if time + kr >= st && time < (st + len) then a else mempty

skewedTriangle01 :: Number -> Number -> Number -> Number
skewedTriangle01 os len = lcmap (_ % len) go
  where
  go time
    | time < (len * os) = (time / (len * os))
    | otherwise = (len - time) / (len * (1.0 - os))

triangle01 :: Number -> Number -> Number
triangle01 = skewedTriangle01 0.5

toNel :: forall s. Semiring s => List s -> NonEmpty List s
toNel Nil = zero :| Nil

toNel (h : t) = h :| t

wah :: String -> String -> Number -> Int -> List Number -> (Number -> Number) -> (Number -> Number) -> Number -> AudioUnit D2
wah tag pwave len nwahs pitches gnF panF time = pannerMono_ (tag <> "WahPanner") (panF time) (gain_ (tag <> "WahGain") (if time >= len then 0.0 else (gnF time * (triangle01 (len / (toNumber nwahs)) time) / (toNumber $ L.length pitches))) (toNel (L.mapWithIndex (\i p -> periodicOsc_ (tag <> show i) pwave (conv440 p)) pitches)))

----
kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

krt = kr * tempo / 60.0 :: Number

nkrt = -1.0 * krt :: Number

loopDownload :: AudioContext -> String -> Aff BrowserAudioBuffer
loopDownload ctx str =
  res
    >>= either
        ( \e -> do
            delay (Milliseconds 20.0)
            loopDownload ctx str
        )
        pure
  where
  res = try $ toAffE (decodeAudioDataFromUri ctx str)

makeBuffersUsingCache :: (O.Object BrowserAudioBuffer -> Tuple (Array (Tuple String String)) (O.Object BrowserAudioBuffer)) -> AudioContext -> O.Object BrowserAudioBuffer -> (O.Object BrowserAudioBuffer -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
makeBuffersUsingCache bf ctx prev' =
  affable do
    sequential
      ( O.union <$> (pure prev)
          <*> ( sequence
                $ O.fromFoldable
                    ( map
                        ( over _2
                            (parallel <<< loopDownload ctx)
                        )
                        (filter (not <<< flip O.member prev <<< fst) newB)
                    )
            )
      )
  where
  (Tuple newB prev) = bf prev'

makeBuffersKeepingCache :: Array (Tuple String String) -> AudioContext -> O.Object BrowserAudioBuffer -> (O.Object BrowserAudioBuffer -> Effect Unit) -> (Error -> Effect Unit) -> Effect Unit
makeBuffersKeepingCache = makeBuffersUsingCache <<< Tuple

-----------------------
------------------
-----------
------
--
epwf :: Array (Tuple Number Number) -> Number -> AudioParameter
epwf = evalPiecewise kr

data PlayerEvent
  = Triangle (Vec D3 (Maybe Number))
  | Square (Vec D4 (Maybe Number))
  | Motion (Maybe Point) (Either Point (Tuple Point Point)) -- resting point or offset from mouse
  | Rise (Vec D6 (Maybe Number)) -- pos, stopped
  | Large (List (Tuple Point Number)) -- pos, startT
  | Bells (List (List Number))
  | Gears (Vec D4 (Maybe Number))
  | Shrink (Vec D6 (Number))
  | Snow (List (Maybe Number)) -- time
  | Heart (Maybe Number)
  | NoEvent Number -- time

derive instance eqPlayerEvent :: Eq PlayerEvent

type SilentNightPlayerT
  = { verse :: Verse
    , verseOne :: VerseChoice
    , verseTwo :: VerseChoice
    , verseThree :: VerseChoice
    , playerEvents :: Array PlayerEvent
    , eventStart :: Number
    }

data Activity
  = Intro
  | HarmChooser { step :: HarmChooserStep }
  | SilentNightPlayer SilentNightPlayerT

derive instance eqActivity :: Eq Activity

data Verse
  = Verse1
  | Verse2
  | Verse3

derive instance eqVerse :: Eq Verse

data VerseChoice
  = VersionOne
  | VersionTwo
  | VersionThree
  | VersionFour
  | VersionFive
  | VersionSix
  | VersionSeven
  | VersionEight

verseChoices =
  [ VersionOne
  , VersionTwo
  , VersionThree
  , VersionFour
  , VersionFive
  , VersionSix
  , VersionSeven
  , VersionEight
  ] ::
    Array VerseChoice

derive instance eqVerseChoice :: Eq VerseChoice

versionToInt :: VerseChoice -> Int
versionToInt VersionOne = 0

versionToInt VersionTwo = 1

versionToInt VersionThree = 2

versionToInt VersionFour = 3

versionToInt VersionFive = 4

versionToInt VersionSix = 5

versionToInt VersionSeven = 6

versionToInt VersionEight = 7

chooseVerseOne :: VerseChoice -> SilentNightAccumulator -> Number -> MakeCanvasT
chooseVerseOne vc acc time =
  makeCanvas
    ( acc
        { verseStarts =
          acc.verseStarts
            { one = Just $ Tuple time vc
            }
        , activity = HarmChooser { step: Row2Animation { startsAt: time, verseOne: vc } }
        }
    )
    time

chooseVerseTwo :: VerseChoice -> VerseChoice -> SilentNightAccumulator -> Number -> MakeCanvasT
chooseVerseTwo v1 vc acc time =
  makeCanvas
    ( acc
        { verseStarts =
          acc.verseStarts
            { two = Just $ Tuple time vc
            }
        , activity = HarmChooser { step: Row3Animation { startsAt: time, verseOne: v1, verseTwo: vc } }
        }
    )
    time

chooseVerseThree :: VerseChoice -> VerseChoice -> VerseChoice -> SilentNightAccumulator -> Number -> MakeCanvasT
chooseVerseThree v1 v2 vc acc time =
  makeCanvas
    ( acc
        { introEnds = Just time
        , verseStarts =
          acc.verseStarts
            { three = Just $ Tuple time vc
            }
        , activity = HarmChooser { step: FadeOutAnimation { startsAt: time, verseOne: v1, verseTwo: v2, verseThree: vc } }
        }
    )
    time

makeCircleDim :: Number -> Number -> Number
makeCircleDim w i = calcSlope 0.0 (w / circleDivisor) 7.0 (w / (circleDivisor * 2.0)) i

doVAction :: SilentNightAccumulator -> Number -> Number -> Number -> VerseChoice -> Boolean
doVAction acc w fullH h vc =
  let
    i = (toNumber <<< versionToInt) vc

    dim = makeCircleDim (min w fullH) i

    twoDim = 2.0 * dim

    out =
      doAction acc
        { x: ((2.0 * i + 1.0) * w / 16.0) - dim
        , y: h - dim
        , width: twoDim
        , height: twoDim
        }
  in
    out

verseVersionChooser :: (VerseChoice -> SilentNightAccumulator -> Number -> MakeCanvasT) -> Number -> Number -> Number -> Drawing -> SilentNightAccumulator -> Number -> MakeCanvasT
verseVersionChooser vc w fullH h d acc time
  | doVAction acc w fullH h VersionOne = vc VersionOne acc time
  | doVAction acc w fullH h VersionTwo = vc VersionTwo acc time
  | doVAction acc w fullH h VersionThree = vc VersionThree acc time
  | doVAction acc w fullH h VersionFour = vc VersionFour acc time
  | doVAction acc w fullH h VersionFive = vc VersionFive acc time
  | doVAction acc w fullH h VersionSix = vc VersionSix acc time
  | doVAction acc w fullH h VersionSeven = vc VersionSeven acc time
  | doVAction acc w fullH h VersionEight = vc VersionEight acc time
  | otherwise = pure $ Tuple acc (d <> circles fullH w h (const 1.0) 1.0)

data HarmChooserStep
  = Row1Animation { startsAt :: Number }
  | Row1Choose
  | Row2Animation { startsAt :: Number, verseOne :: VerseChoice }
  | Row2Choose { verseOne :: VerseChoice }
  | Row3Animation { startsAt :: Number, verseOne :: VerseChoice, verseTwo :: VerseChoice }
  | Row3Choose { verseOne :: VerseChoice, verseTwo :: VerseChoice }
  | FadeOutAnimation { startsAt :: Number, verseOne :: VerseChoice, verseTwo :: VerseChoice, verseThree :: VerseChoice }

derive instance eqHarmChooserStep :: Eq HarmChooserStep

amark ::
  forall p ami amo rest.
  Strong p =>
  p ami amo ->
  p
    { audioMarkers :: ami
    | rest
    }
    { audioMarkers :: amo
    | rest
    }
amark = prop (SProxy :: SProxy "audioMarkers")

type AccumulatorSetter a
  = a -> SilentNightAccumulator -> SilentNightAccumulator

type EndTimeSetter
  = AccumulatorSetter Number

type BegTimeSetter
  = AccumulatorSetter Number

type AccumulatorGetter a
  = AudioEnv -> (Maybe a)

type BegTimeGetter
  = AccumulatorGetter (Number)

type EndTimeGetter
  = AccumulatorGetter (Number)

_Just_2_1 = _Just <<< _2 :: forall p inter. Choice p ⇒ Strong p ⇒ p (Maybe (Tuple inter (Maybe Number))) (Maybe (Tuple inter (Maybe Number))) -> p (Marker inter) (Marker inter)

_Just_2_2 = _Just <<< _2 <<< _Just <<< _2 :: forall p inter. Choice p ⇒ Strong p ⇒ p (Maybe Number) (Maybe Number) -> p (Marker inter) (Marker inter)

maybeTupleMod :: forall i x. i -> Maybe (Tuple i (Maybe x)) -> Maybe (Tuple i (Maybe x))
maybeTupleMod i Nothing = Just $ Tuple i Nothing

maybeTupleMod i (Just (Tuple _ x)) = Just $ Tuple i x

standardEndTimeBleed = 5.0 :: Number

triangleLens = amark <<< prop (SProxy :: SProxy "triangle")

triangleEndTimeBleed = standardEndTimeBleed :: Number

--getBegTime :: forall inter r1. (Getter' { audioMarkers :: AudioMarkers | r1 } (Marker inter)) -> SilentNightAccumulator -> Maybe Number
getBegTime interLens = preview (interLens <<< _Just <<< _1)

getInter interLens = preview (interLens <<< _Just <<< _2 <<< _Just <<< _1)

--getEndTime :: forall inter r1. (Getter' { audioMarkers :: AudioMarkers | r1 } (Marker inter)) -> SilentNightAccumulator -> Maybe Number
getEndTime interLens = preview (interLens <<< _Just <<< _2 <<< _Just <<< _2 <<< _Just)

--setBegTime :: forall inter r1. (Setter' { audioMarkers :: AudioMarkers | r1 } (Marker inter)) -> Number -> SilentNightAccumulator -> SilentNightAccumulator
setBegTime interLens = over interLens <<< maybeTupleMod

--setInter :: forall inter r1. (Setter' { audioMarkers :: AudioMarkers | r1 } (Marker inter)) -> inter -> SilentNightAccumulator -> SilentNightAccumulator
setInter interLens = over (interLens <<< _Just_2_1) <<< maybeTupleMod

---setEndTime :: forall inter r1. (Setter' { audioMarkers :: AudioMarkers | r1 } (Marker inter)) -> Number -> SilentNightAccumulator -> SilentNightAccumulator
setEndTime endTimeLens = set (endTimeLens <<< _Just_2_2) <<< Just

getTriangleBegTime = getBegTime triangleLens :: BegTimeGetter

getTriangleVector = getInter triangleLens :: AccumulatorGetter (Vec D3 (Maybe Number))

getTriangleEndTime = getEndTime triangleLens :: EndTimeGetter

setTriangleBegTime = setBegTime triangleLens :: BegTimeSetter

setTriangleVector = setInter triangleLens :: AccumulatorSetter (Vec D3 (Maybe Number))

setTriangleEndTime = setEndTime triangleLens :: EndTimeSetter

squareLens = amark <<< prop (SProxy :: SProxy "square")

squareEndTimeBleed = standardEndTimeBleed :: Number

getSquareBegTime = getBegTime squareLens :: BegTimeGetter

getSquareVector = getInter squareLens :: AccumulatorGetter (Vec D4 (Maybe Number))

getSquareEndTime = getEndTime squareLens :: EndTimeGetter

setSquareBegTime = setBegTime squareLens :: BegTimeSetter

setSquareVector = setInter squareLens :: AccumulatorSetter (Vec D4 (Maybe Number))

setSquareEndTime = setEndTime squareLens :: EndTimeSetter

motionEndTimeBleed = standardEndTimeBleed :: Number

motionLens = amark <<< prop (SProxy :: SProxy "motion")

getMotionBegTime = getBegTime motionLens :: BegTimeGetter

getMotionPoints = getInter motionLens :: AccumulatorGetter (Tuple Point Point)

getMotionEndTime = getEndTime motionLens :: EndTimeGetter

setMotionBegTime = setBegTime motionLens :: BegTimeSetter

normalizePoints :: Number -> Number -> Tuple Point Point -> Tuple Point Point
normalizePoints w h (Tuple { x: x0, y: y0 } { x: x1, y: y1 }) = Tuple { x: x0 / w, y: y0 / h } { x: x1 / w, y: y1 / h }

setMotionPoints w h p acc = setInter motionLens (normalizePoints w h p) acc -- :: Number -> Number -> AccumulatorSetter (Tuple Point Point)

setMotionEndTime = setEndTime motionLens :: EndTimeSetter

riseEndTimeBleed = standardEndTimeBleed :: Number

riseLens = amark <<< prop (SProxy :: SProxy "rise")

getRiseBegTime = getBegTime riseLens :: BegTimeGetter

getRiseVector = getInter riseLens :: AccumulatorGetter (Vec D6 (Maybe Number))

getRiseEndTime = getEndTime riseLens :: EndTimeGetter

setRiseBegTime = setBegTime riseLens :: BegTimeSetter

setRiseVector = setInter riseLens :: AccumulatorSetter (Vec D6 (Maybe Number))

setRiseEndTime = setEndTime riseLens :: EndTimeSetter

largeEndTimeBleed = standardEndTimeBleed :: Number

largeLens = amark <<< prop (SProxy :: SProxy "large")

getLargeBegTime = getBegTime largeLens :: BegTimeGetter

getLargeList = getInter largeLens :: AccumulatorGetter (List Number)

getLargeEndTime = getEndTime largeLens :: EndTimeGetter

setLargeBegTime = setBegTime largeLens :: BegTimeSetter

setLargeList = setInter largeLens :: AccumulatorSetter (List Number)

setLargeEndTime = setEndTime largeLens :: EndTimeSetter

bellsEndTimeBleed = standardEndTimeBleed :: Number

bellsLens = amark <<< prop (SProxy :: SProxy "bells")

getBellsBegTime = getBegTime bellsLens :: BegTimeGetter

getBellsList = getInter bellsLens :: AccumulatorGetter (List (List Number))

getBellsEndTime = getEndTime bellsLens :: EndTimeGetter

setBellsBegTime = setBegTime bellsLens :: BegTimeSetter

setBellsList = setInter bellsLens :: AccumulatorSetter (List (List Number))

setBellsEndTime = setEndTime bellsLens :: EndTimeSetter

gearsEndTimeBleed = standardEndTimeBleed :: Number

gearsLens = amark <<< prop (SProxy :: SProxy "gears")

getGearsBegTime = getBegTime gearsLens :: BegTimeGetter

getGearsVector = getInter gearsLens :: AccumulatorGetter (Vec D4 (Maybe Number))

getGearsEndTime = getEndTime gearsLens :: EndTimeGetter

setGearsBegTime = setBegTime gearsLens :: BegTimeSetter

setGearsVector = setInter gearsLens :: AccumulatorSetter (Vec D4 (Maybe Number))

setGearsEndTime = setEndTime gearsLens :: EndTimeSetter

shrinkEndTimeBleed = standardEndTimeBleed :: Number

shrinkLens = amark <<< prop (SProxy :: SProxy "shrink")

getShrinkBegTime = getBegTime shrinkLens :: BegTimeGetter

getShrinkVector = getInter shrinkLens :: AccumulatorGetter (Vec D6 Number)

getShrinkEndTime = getEndTime shrinkLens :: EndTimeGetter

setShrinkBegTime = setBegTime shrinkLens :: BegTimeSetter

setShrinkVector = setInter shrinkLens :: AccumulatorSetter (Vec D6 Number)

setShrinkEndTime = setEndTime shrinkLens :: EndTimeSetter

snowEndTimeBleed = standardEndTimeBleed :: Number

snowLens = amark <<< prop (SProxy :: SProxy "snow")

getSnowBegTime = getBegTime snowLens :: BegTimeGetter

getSnowList = getInter snowLens :: AccumulatorGetter (List (Maybe Number))

getSnowEndTime = getEndTime snowLens :: EndTimeGetter

setSnowBegTime = setBegTime snowLens :: BegTimeSetter

setSnowList = setInter snowLens :: AccumulatorSetter (List (Maybe Number))

setSnowEndTime = setEndTime snowLens :: EndTimeSetter

heartEndTimeBleed = standardEndTimeBleed :: Number

heartLens = amark <<< prop (SProxy :: SProxy "heart")

getHeartBegTime = getBegTime heartLens :: BegTimeGetter

getHeartStartTime = getInter heartLens :: AccumulatorGetter Number

getHeartEndTime = getEndTime heartLens :: EndTimeGetter

setHeartBegTime = setBegTime heartLens :: BegTimeSetter

setHeartStartTime = setInter heartLens :: AccumulatorSetter Number

setHeartEndTime = setEndTime heartLens :: EndTimeSetter

type Marker a
  = Maybe (Tuple Number (Maybe (Tuple a (Maybe Number))))

type VecMarker a
  = Marker (Vec a (Maybe Number))

type VecMarker' a
  = Marker (Vec a Number)

type TriangleMarker
  = VecMarker D3 -- startPts end

type SquareMarker
  = VecMarker D4 -- startPts end

type MotionMarker
  = Marker (Tuple Point Point) -- motion end

type RiseMarker
  = VecMarker D6 -- startPts end

type LargeMarker
  = Marker (List Number) -- onsets end

type ShrinkMarker
  = VecMarker' D6 -- circleRs end

type BellsMarker
  = Marker (List (List Number)) -- onsets end

type GearsMarker
  = VecMarker D4 -- startPts end

type SnowMarker
  = Marker (List (Maybe Number)) -- onsets end

type HeartMarker
  = Marker Number

type AudioMarkers
  = { triangle :: TriangleMarker
    , square :: SquareMarker
    , motion :: MotionMarker
    , rise :: RiseMarker
    , large :: LargeMarker
    , bells :: BellsMarker
    , gears :: GearsMarker
    , shrink :: ShrinkMarker
    , snow :: SnowMarker
    , heart :: HeartMarker
    }

defaultAudioMarkers :: AudioMarkers
defaultAudioMarkers =
  { triangle: Nothing
  , square: Nothing
  , motion: Nothing
  , rise: Nothing
  , large: Nothing
  , bells: Nothing
  , gears: Nothing
  , shrink: Nothing
  , snow: Nothing
  , heart: Nothing
  }

type SilentNightAccumulator
  = { initiatedClick :: Boolean
    , inClick :: Boolean
    , curClickId :: Maybe Int
    , mousePosition :: Maybe { x :: Number, y :: Number }
    , activity :: Activity
    , initiatedCoda :: Boolean
    , verseStarts :: VerseStarts
    , mainStarts :: Maybe Number
    , introEnds :: Maybe Number
    , audioMarkers :: AudioMarkers
    }

inRect :: Point -> Rectangle -> Boolean
inRect p r = p.x >= r.x && p.y >= r.y && p.x <= (r.x + r.width) && p.y <= (r.y + r.height)

doAction :: SilentNightAccumulator -> Rectangle -> Boolean
doAction acc r = acc.initiatedClick && (maybe false (flip inRect r) acc.mousePosition)

doingAction :: SilentNightAccumulator -> Rectangle -> Boolean
doingAction acc r = acc.inClick && (maybe false (flip inRect r) acc.mousePosition)

boldItalic :: FontOptions
boldItalic = bold <> italic

fadeIn = 2.0 :: Number

stay = 3.0 :: Number

fadeOut = 2.0 :: Number

dark = 0.5 :: Number

darknessFadeIn = 0.0 :: Number

silentNightFadeIn = darknessFadeIn + fadeIn :: Number

silentNightStay = silentNightFadeIn + fadeIn :: Number

silentNightFadeOut = silentNightStay + stay :: Number

silentNightDark = silentNightFadeOut + fadeOut :: Number

instructionFadeIn = silentNightDark + dark :: Number

instructionStay = instructionFadeIn + fadeIn :: Number

instructionFadeOut = instructionStay + stay :: Number

instructionDark = instructionFadeOut + fadeOut :: Number

circleIntro = 3.0 :: Number

circleFade = 0.5 :: Number

circleFlyAway = 5.0 :: Number

calcSlope :: Number -> Number -> Number -> Number -> Number -> Number
calcSlope x0 y0 x1 y1 x =
  if x1 == x0 || y1 == y0 then
    y0
  else
    let
      m = (y1 - y0) / (x1 - x0)

      b = y0 - m * x0
    in
      m * x + b

introOpacity :: Number -> Number
introOpacity time
  | time < silentNightFadeIn = calcSlope darknessFadeIn 0.0 silentNightFadeIn 0.0 time
  | time < silentNightStay = calcSlope silentNightFadeIn 0.0 silentNightStay 1.0 time
  | time < silentNightFadeOut = calcSlope silentNightStay 1.0 silentNightFadeOut 1.0 time
  | time < silentNightDark = calcSlope silentNightFadeOut 1.0 silentNightDark 0.0 time
  | time < instructionFadeIn = calcSlope silentNightDark 0.0 instructionFadeIn 0.0 time
  | time < instructionStay = calcSlope instructionFadeIn 0.0 instructionStay 1.0 time
  | time < instructionFadeOut = calcSlope instructionStay 1.0 instructionFadeOut 1.0 time
  | time < instructionDark = calcSlope instructionFadeOut 1.0 instructionDark 0.0 time
  | otherwise = 0.0

circleDivisor = 18.0 :: Number

whiteRGBA = rgba 255 255 255 :: Number -> Color

nCurve :: Number -> Number
nCurve = pnCurve 1.0

pnCurve :: Number -> Number -> Number
pnCurve p n
  | n < 0.5 = ((2.0 * n) `pow` p) / 2.0
  | otherwise = ((((n - 0.5) * 2.0) `pow` (1.0 / p)) / 2.0) + 0.5

circles :: Number -> Number -> Number -> (VerseChoice -> Number) -> Number -> Drawing
circles fH w h opq traj = fold (map (\i' -> let i = (toNumber <<< versionToInt) i' in filled (fillColor (whiteRGBA (opq i'))) (circle ((2.0 * i * traj + 1.0) * w / 16.0) h (makeCircleDim (min w fH) i))) verseChoices)

circles' :: Number -> Number -> Number -> (Number -> Number) -> Drawing
circles' fH w h traj = fold (map (\i' -> let i = (toNumber <<< versionToInt) i' in filled (fillColor (whiteRGBA 1.0)) (circle ((2.0 * i + 1.0) * w / 16.0) h (makeCircleDim (min w fH) i * (traj i)))) verseChoices)

circleFanner :: Number -> Number -> Number -> Number -> Number -> Drawing
circleFanner fH w h startsAt time =
  let
    nTime = time - startsAt
  in
    circles' fH w h
      ( \i ->
          let
            st = (pnCurve 0.7 $ i / 10.0) * circleIntro

            ed = (pnCurve 0.7 $ (i + 2.0) / 10.0) * circleIntro

            x
              | nTime < st = 0.0
              | nTime >= ed = 1.0
              | otherwise = pnCurve 1.2 $ min 1.0 (max 0.0 (calcSlope st 0.0 ed 1.0 nTime))
          in
            x
      )

circleChoice :: Number -> Number -> Number -> VerseChoice -> Number -> Number -> Drawing
circleChoice fH w h vc startsAt time = circles fH w h (\v -> if v == vc then 1.0 else max 0.3 (1.0 - (time - startsAt) / (0.7 * circleFade))) 1.0

firstRow :: Number -> Number
firstRow h = h / 6.0

secondRow :: Number -> Number
secondRow h = 3.0 * h / 6.0

thirdRow :: Number -> Number
thirdRow h = 5.0 * h / 6.0

circleChosen :: Number -> Number -> Number -> VerseChoice -> Drawing
circleChosen fH w h vc = circles fH w h (\v -> if v == vc then 1.0 else 0.3) 1.0

circleOutroStart :: Number -> Number -> Verse -> VerseChoice -> Tuple Number Number
circleOutroStart w h Verse1 VersionOne = Tuple (1.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionTwo = Tuple (3.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionThree = Tuple (5.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionFour = Tuple (7.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionFive = Tuple (9.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionSix = Tuple (11.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionSeven = Tuple (13.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse1 VersionEight = Tuple (15.0 * w / 16.0) (firstRow h)

circleOutroStart w h Verse2 VersionOne = Tuple (1.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionTwo = Tuple (3.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionThree = Tuple (5.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionFour = Tuple (7.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionFive = Tuple (9.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionSix = Tuple (11.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionSeven = Tuple (13.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse2 VersionEight = Tuple (15.0 * w / 16.0) (secondRow h)

circleOutroStart w h Verse3 VersionOne = Tuple (1.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionTwo = Tuple (3.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionThree = Tuple (5.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionFour = Tuple (7.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionFive = Tuple (9.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionSix = Tuple (11.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionSeven = Tuple (13.0 * w / 16.0) (thirdRow h)

circleOutroStart w h Verse3 VersionEight = Tuple (15.0 * w / 16.0) (thirdRow h)

circleOutroEnd :: Number -> Number -> Verse -> VerseChoice -> Tuple Number Number
circleOutroEnd w h Verse1 VersionOne = Tuple (w * 0.29) (h * 0.91)

circleOutroEnd w h Verse1 VersionTwo = Tuple (w * 0.45) (h * 0.23)

circleOutroEnd w h Verse1 VersionThree = Tuple (w * 0.02) (h * 0.45)

circleOutroEnd w h Verse1 VersionFour = Tuple (w * 0.23) (h * 0.34)

circleOutroEnd w h Verse1 VersionFive = Tuple (w * 0.96) (h * 0.77)

circleOutroEnd w h Verse1 VersionSix = Tuple (w * 0.72) (h * 0.51)

circleOutroEnd w h Verse1 VersionSeven = Tuple (w * 0.01) (h * 0.13)

circleOutroEnd w h Verse1 VersionEight = Tuple (w * 0.60) (h * 0.03)

circleOutroEnd w h Verse2 VersionOne = Tuple (w * 0.18) (h * 0.76)

circleOutroEnd w h Verse2 VersionTwo = Tuple (w * 0.14) (h * 0.54)

circleOutroEnd w h Verse2 VersionThree = Tuple (w * 0.85) (h * 0.38)

circleOutroEnd w h Verse2 VersionFour = Tuple (w * 0.36) (h * 0.65)

circleOutroEnd w h Verse2 VersionFive = Tuple (w * 0.72) (h * 0.41)

circleOutroEnd w h Verse2 VersionSix = Tuple (w * 0.3) (h * 0.03)

circleOutroEnd w h Verse2 VersionSeven = Tuple (w * 0.9) (h * 0.57)

circleOutroEnd w h Verse2 VersionEight = Tuple (w * 0.05) (h * 0.98)

circleOutroEnd w h Verse3 VersionOne = Tuple (w * 0.21) (h * 0.82)

circleOutroEnd w h Verse3 VersionTwo = Tuple (w * 0.36) (h * 0.55)

circleOutroEnd w h Verse3 VersionThree = Tuple (w * 0.45) (h * 0.45)

circleOutroEnd w h Verse3 VersionFour = Tuple (w * 0.08) (h * 0.13)

circleOutroEnd w h Verse3 VersionFive = Tuple (w * 0.72) (h * 0.86)

circleOutroEnd w h Verse3 VersionSix = Tuple (w * 0.63) (h * 0.34)

circleOutroEnd w h Verse3 VersionSeven = Tuple (w * 0.83) (h * 0.75)

circleOutroEnd w h Verse3 VersionEight = Tuple (w * 0.02) (h * 0.99)

circleOutro :: Verse -> VerseChoice -> Boolean -> Number -> Number -> Number -> Number -> Drawing
circleOutro vs vc chosen w h startsAt time =
  let
    opq' = if chosen then 1.0 else 0.3

    opq = max 0.0 $ opq' - (opq' * (time - startsAt) / circleFlyAway)

    i = (toNumber <<< versionToInt) vc

    (Tuple x0 y0) = circleOutroStart w h vs vc

    (Tuple x1 y1) = circleOutroEnd w h vs vc

    x = calcSlope 0.0 x0 1.0 x1 (pnCurve 0.8 $ (time - startsAt) / circleFlyAway)

    y = calcSlope 0.0 y0 1.0 y1 (pnCurve 0.8 $ (time - startsAt) / circleFlyAway)
  in
    filled (fillColor (whiteRGBA opq)) (circle x y (makeCircleDim (min w h) i))

type MakeCanvasT
  = Reader { evts :: Array PlayerEvent, w :: Number, h :: Number } (Tuple SilentNightAccumulator Drawing)

standardIntro = 1.4 :: Number

standardOutro = 4.0 :: Number

standardPress = 0.5 :: Number

motionNormal = 12.0 :: Number

bellsNormal = 18.0 :: Number

riseNormal = 12.0 :: Number

gearStay = 4.0 :: Number

shrinkNormal = 18.0 :: Number

squareTravel = 1.0 :: Number

heartNormal = 10.0 :: Number

snowMin = 6.0 :: Number

snowMax = 24.0 :: Number

snowDiff = snowMax - snowMin :: Number

largeCrossing = 25.0 :: Number

snowYp :: Number -> Number -> Number -> Number
snowYp h nowT v = h * (min 1.1 $ calcSlope 0.0 (-0.1) (snowMin + (v * snowDiff)) (1.1) nowT)

snowXp :: Number -> Number -> Number
snowXp w v = w * v

snowRad :: Number -> Number -> Number -> Number
snowRad w h v = (min w h) * 0.05 * v

pressEffect :: Number -> Number -> Number -> Number
pressEffect cw press time
  | time < press / 2.0 = cw + (time * cw * 0.1 / press)
  | time < press = cw + ((press - time) * cw * 0.1 / press)
  | otherwise = cw

newCanvas :: SilentNightPlayerT -> SilentNightAccumulator -> Number -> MakeCanvasT
newCanvas i acc time =
  makeCanvas
    ( acc
        { activity =
          SilentNightPlayer
            ( i
                { playerEvents = drop 1 i.playerEvents
                , eventStart = time
                }
            )
        }
    )
    time

sinp :: Number -> Number -> Number
sinp v p = (v * (((sin (p)) * 0.5 + 0.5) * 0.7 + 0.15))

cosp :: Number -> Number -> Number
cosp v p = (v * (((cos (p)) * 0.5 + 0.5) * 0.7 + 0.15))

sqToRect :: Number -> Number -> Number -> Rectangle
sqToRect x y r = { x: x - r, y: y - r, width: 2.0 * r, height: 2.0 * r }

nextObj :: forall a. (a -> PlayerEvent) -> SilentNightAccumulator -> SilentNightPlayerT -> (Number -> a) -> Number -> MakeCanvasT
nextObj pef acc i tf time =
  makeCanvas
    ( acc
        { activity =
          SilentNightPlayer
            ( i
                { playerEvents = [ pef (tf time) ] <> drop 1 i.playerEvents
                }
            )
        }
    )
    time

mouseOrBust :: Maybe Point -> Point
mouseOrBust = fromMaybe { x: 0.0, y: 0.0 }

riseXP = [ 1.0 / 12.0, 3.0 / 12.0, 5.0 / 12.0, 7.0 / 12.0, 9.0 / 12.0, 11.0 / 12.0 ] :: Array Number

calibrateX :: SilentNightAccumulator -> Point -> Number
calibrateX acc v = (mouseOrBust acc.mousePosition).x - v.x

calibrateY :: SilentNightAccumulator -> Point -> Number
calibrateY acc v = (mouseOrBust acc.mousePosition).y - v.y

eix :: Number -> SilentNightAccumulator -> Either Point (Tuple Point Point) -> Number
eix w acc = either (\v -> w * v.x) (calibrateX acc <<< snd)

eiy :: Number -> SilentNightAccumulator -> Either Point (Tuple Point Point) -> Number
eiy h acc = either (\v -> h * v.y) (calibrateY acc <<< snd)

nothingize :: forall a. a -> Maybe a
nothingize = const Nothing

needsToFollow :: forall a b. Number -> Number -> Number -> SilentNightAccumulator -> Either a b -> Boolean
needsToFollow xp yp cw acc =
  either
    ( \_ ->
        doingAction
          acc
          (sqToRect xp yp cw)
    )
    (const false)

needsToStopFollowing :: forall a b. SilentNightAccumulator -> Either a b -> Boolean
needsToStopFollowing acc =
  either
    (const false)
    (const $ not acc.inClick)

bFoldL :: forall a. (Int -> a -> Boolean) -> List a -> Boolean
bFoldL f l = go 0 l
  where
  go _ Nil = false

  go x (a : b) = if f x a then true else go (x + 1) b

snowRecurser :: SilentNightPlayerT -> Number -> Number -> SilentNightAccumulator -> Number -> Number -> List (Maybe Number) -> MakeCanvasT
snowRecurser i w h acc startT time l = go 0 Nil (if acc.inClick then l else Nil)
  where
  go z hd Nil =
    pure
      $ Tuple (setSnowList l (setSnowBegTime i.eventStart acc))
          ( fold
              ( map
                  ( \(Tuple (SnowI xrd yrd rrd) mn) ->
                      if mn == Nothing then
                        filled (fillColor (whiteRGBA 1.0))
                          (circle (snowXp w xrd) (snowYp h (time - startT) yrd) (snowRad w h rrd))
                      else
                        mempty
                  )
                  (A.zipWith Tuple snows (A.fromFoldable l))
              )
          )

  go z hd (a : b) =
    if a == Nothing
      && maybe false
          ( \(SnowI xrd yrd rrd) ->
              doAction
                acc
                (sqToRect (snowXp w xrd) (snowYp h (time - startT) yrd) (snowRad w h rrd))
          )
          (A.index snows z) then
      makeCanvas
        ( acc
            { activity =
              SilentNightPlayer
                ( i
                    { playerEvents = [ Snow (hd <> (pure $ Just time) <> b) ] <> drop 1 i.playerEvents
                    }
                )
            }
        )
        time
    else
      go (z + 1) (hd <> pure a) b

bellsInternal = standardIntro + bellsNormal :: Number

bellsFaded = bellsInternal + standardOutro :: Number

bellsRecurser :: SilentNightPlayerT -> Number -> Number -> SilentNightAccumulator -> Number -> Number -> List (List Number) -> MakeCanvasT
bellsRecurser i w h acc startT time l = go 0 Nil (if acc.inClick then l else Nil)
  where
  op = max 0.0 (min 1.0 (calcSlope (i.eventStart + bellsInternal) 1.0 (i.eventStart + bellsFaded) 0.0 time))

  notNow Nil = true

  notNow (a : b) = a /= time

  go z hd Nil =
    pure
      $ Tuple (setBellsList l acc)
          ( makeBells w h
              ( map
                  ( \tl ->
                      Tuple
                        ( case tl of
                            Nil -> 1.0
                            (a : b) -> if time - a < 0.25 then (calcSlope a 1.0 (a + 0.25) 1.13 time) else if time - a < 0.5 then (calcSlope (a + 0.25) 1.13 (a + 0.5) 1.0 time) else 1.0
                        )
                        op
                  )
                  (A.fromFoldable l)
              )
          )

  go z hd (a : b) =
    if notNow a
      && doAction
          acc
          (sqToRect (bellX z * w) (bellY z * h) ((min w h) / 24.0)) then
      makeCanvas
        ( acc
            { activity =
              SilentNightPlayer
                ( i
                    { playerEvents = [ Bells (hd <> (pure $ time : a) <> b) ] <> drop 1 i.playerEvents
                    }
                )
            }
        )
        time
    else
      go (z + 1) (hd <> pure a) b

bellX :: Int -> Number
bellX i = ((toNumber (i `mod` 6)) * 2.0 + 1.0) / 12.0

bellY :: Int -> Number
bellY i = ((toNumber (i `div` 6)) * 2.0 + 1.0) / 8.0

-- sizeMult opacity
makeBells :: Number -> Number -> Array (Tuple Number Number) -> Drawing
makeBells w h a =
  fold
    ( A.mapWithIndex
        ( \i (Tuple sizeM op) ->
            filled (fillColor (whiteRGBA op))
              (circle (bellX i * w) (bellY i * h) (sizeM * (min w h) / 24.0))
        )
        a
    )

isLarge :: SilentNightAccumulator -> Boolean
isLarge { activity: SilentNightPlayer { playerEvents } } =
  maybe false
    ( \v -> case v of
        Large _ -> true
        _ -> false
    )
    $ head playerEvents

isLarge _ = false

heartbeat :: Number -> Number -> Number -> Number -> Number -> Number -> Number
heartbeat st b0 s0 b1 s1 t = go (t % pd)
  where
  pd = st + b0 + s0 + b1 + s1

  go i
    | i < st = 0.0
    | i < b0 + st = sin ((i - st) * pi / b0)
    | i < s0 + b0 + st = 0.0
    | i < b1 + s0 + b0 + st = 0.7 * sin ((i - b0 - s0 - st) * pi / b1)
    | otherwise = 0.0

epsilon = 0.01 :: Number

eps :: Number -> Number -> Boolean
eps a b = abs (a - b) < epsilon

targetMaker :: forall a. Vec a Number -> Vec a (Number -> Number)
targetMaker v = map (\n i -> if (eps n i) then i else i + ((n - i) * kr * 1.4)) v

---- shrink vars
targetsOne = targetMaker (0.2 +> 0.3 +> 0.4 +> 0.1 +> 0.07 +> 0.17 +> empty) :: Vec D6 (Number -> Number)

targetsTwo = targetMaker (0.51 +> 0.13 +> 0.39 +> 0.26 +> 0.4 +> 0.04 +> empty) :: Vec D6 (Number -> Number)

targetsThree = targetMaker (0.04 +> 0.47 +> 0.06 +> 0.39 +> 0.1 +> 0.52 +> empty) :: Vec D6 (Number -> Number)

targetsFour = targetMaker (0.13 +> 0.08 +> 0.35 +> 0.43 +> 0.2 +> 0.28 +> empty) :: Vec D6 (Number -> Number)

targetsFive = targetMaker (0.18 +> 0.26 +> 0.5 +> 0.12 +> 0.54 +> 0.3 +> empty) :: Vec D6 (Number -> Number)

targetsSix = targetMaker (0.3 +> 0.05 +> 0.1 +> 0.6 +> 0.3 +> 0.22 +> empty) :: Vec D6 (Number -> Number)

targetsDefault = targetMaker (0.1 +> 0.15 +> 0.6 +> 0.3 +> 0.35 +> 0.05 +> empty) :: Vec D6 (Number -> Number)

shrinkOne = Tuple 0.5 0.6 :: Tuple Number Number

shrinkTwo = Tuple 0.7 0.2 :: Tuple Number Number

shrinkThree = Tuple 0.2 0.8 :: Tuple Number Number

shrinkFour = Tuple 0.3 0.4 :: Tuple Number Number

shrinkFive = Tuple 0.72 0.7 :: Tuple Number Number

shrinkSix = Tuple 0.9 0.8 :: Tuple Number Number

paintShrinks :: Number -> Number -> SilentNightAccumulator -> SilentNightPlayerT -> Vec D6 (Number -> Number) -> Vec D6 Number -> Number -> MakeCanvasT
paintShrinks w h acc i shrinkF curPos time =
  let
    newPos = V.zipWith (\f x -> f x) shrinkF curPos
  in
    pure
      $ ( Tuple
            ( (setShrinkVector newPos acc)
                { activity =
                  SilentNightPlayer
                    ( i
                        { playerEvents = [ Shrink newPos ] <> drop 1 i.playerEvents
                        }
                    )
                }
            )
            $ fold
                ( map
                    ( \(Tuple (Tuple xp yp) r) ->
                        filled
                          ( fillColor
                              (whiteRGBA (min 1.0 (calcSlope (i.eventStart + standardIntro + shrinkNormal) 1.0 (i.eventStart + standardIntro + shrinkNormal + standardOutro) 0.0 time)))
                          )
                          ( circle
                              (w * xp)
                              (h * yp)
                              (r * (min w h) / 2.0)
                          )
                    )
                    (V.zip (shrinkOne +> shrinkTwo +> shrinkThree +> shrinkFour +> shrinkFive +> shrinkSix +> empty) newPos)
                )
        )

----
-- motion
motionMaker :: PlayerEvent -> Number -> Number -> Number -> SilentNightAccumulator -> SilentNightPlayerT -> Drawing -> Number -> MakeCanvasT
motionMaker newM xp yp cw acc i instr time =
  pure
    $ Tuple
        ( acc
            { activity =
              SilentNightPlayer
                ( i
                    { playerEvents = [ newM ] <> drop 1 i.playerEvents
                    }
                )
            }
        )
        ( instr
            <> filled
                ( fillColor
                    ( whiteRGBA
                        ( if time < i.eventStart + standardIntro + motionNormal then
                            1.0
                          else
                            calcSlope (i.eventStart + standardIntro + motionNormal) 1.0 (i.eventStart + standardIntro + motionNormal + standardOutro) 0.0 time
                        )
                    )
                )
                (circle xp yp cw)
        )

gearDir0 = true :: Boolean

gearDir1 = false :: Boolean

gearDir2 = true :: Boolean

gearDir3 = false :: Boolean

gearSpinF :: Number -> Number
gearSpinF t = 0.8 * t + (0.2 * (t `pow` 2.0)) -- t `pow` (1.3 + 0.3 * sin (0.5 * pi * t))

makeCanvas :: SilentNightAccumulator -> Number -> MakeCanvasT
makeCanvas acc time = do
  { w, h } <- ask
  let
    bg = filled (fillColor (rgb 0 0 0)) (rectangle 0.0 0.0 w h)

    introFade = if time > silentNightFadeIn then mempty else filled (fillColor (rgba 255 255 255 (calcSlope 0.0 1.0 silentNightFadeIn 0.0 time))) (rectangle 0.0 0.0 w h)

    starscape = fold (map (\f -> f w h time) starFs)
  map
    (over _2 (\i -> if isLarge acc then bg <> i <> starscape <> introFade else bg <> starscape <> i <> introFade))
    (go w h)
  where
  dAcc = doAction acc

  go :: Number -> Number -> MakeCanvasT
  go w h = case acc.activity of
    Intro ->
      if time > instructionDark then
        makeCanvas (acc { activity = HarmChooser { step: Row1Animation { startsAt: time } } }) time
      else
        pure
          $ Tuple acc
              ( text
                  ( font sansSerif
                      (if time > silentNightDark then 20 else 48)
                      (if time > silentNightDark then mempty else bold)
                  )
                  (w / 2.0 - if time > silentNightDark then 140.0 else 160.0)
                  (h / 2.0)
                  (fillColor (whiteRGBA (introOpacity time)))
                  if time > silentNightDark then "Click on or press the circles" else "Silent Night"
              )
    HarmChooser { step } -> case step of
      Row1Animation i ->
        if time > i.startsAt + circleIntro then
          makeCanvas (acc { activity = HarmChooser { step: Row1Choose } }) time
        else
          pure $ Tuple acc (circleFanner h w (firstRow h) i.startsAt time)
      Row1Choose -> verseVersionChooser chooseVerseOne w h (firstRow h) mempty acc time
      Row2Animation i ->
        if time > i.startsAt + circleIntro then
          makeCanvas (acc { activity = HarmChooser { step: Row2Choose { verseOne: i.verseOne } } }) time
        else
          pure $ Tuple acc (circleFanner h w (secondRow h) i.startsAt time <> circleChoice h w (firstRow h) i.verseOne i.startsAt time)
      Row2Choose i -> verseVersionChooser (chooseVerseTwo i.verseOne) w h (secondRow h) (circleChosen h w (firstRow h) i.verseOne) acc time
      Row3Animation i ->
        if time > i.startsAt + circleIntro then
          makeCanvas (acc { activity = HarmChooser { step: Row3Choose { verseOne: i.verseOne, verseTwo: i.verseTwo } } }) time
        else
          pure $ Tuple acc (circleFanner h w (thirdRow h) i.startsAt time <> circleChoice h w (secondRow h) i.verseTwo i.startsAt time <> circleChosen h w (firstRow h) i.verseOne)
      Row3Choose i -> verseVersionChooser (chooseVerseThree i.verseOne i.verseTwo) w h (thirdRow h) (circleChosen h w (firstRow h) i.verseOne <> circleChosen h w (secondRow h) i.verseTwo) acc time
      FadeOutAnimation i -> do
        { evts } <- ask
        if time > i.startsAt + circleFlyAway then
          makeCanvas
            ( acc
                { activity =
                  SilentNightPlayer
                    { verse: Verse1
                    , verseOne: i.verseOne
                    , verseTwo: i.verseTwo
                    , verseThree: i.verseThree
                    , playerEvents: evts
                    , eventStart: time
                    }
                }
            )
            time
        else
          pure
            $ Tuple acc
                ( fold
                    ( map (\vc -> circleOutro Verse1 vc (vc == i.verseOne) w h i.startsAt time) verseChoices
                        <> map (\vc -> circleOutro Verse2 vc (vc == i.verseTwo) w h i.startsAt time) verseChoices
                        <> map (\vc -> circleOutro Verse3 vc (vc == i.verseThree) w h i.startsAt time) verseChoices
                    )
                )
    SilentNightPlayer i -> case head i.playerEvents of
      Nothing -> pure $ Tuple acc mempty
      (Just evt) -> case evt of
        NoEvent dur ->
          if time > i.eventStart + dur then
            newCanvas i acc time
          else
            pure $ Tuple acc mempty
        Heart v ->
          let
            cw = (min w h) / 9.0

            nextHeart = nextObj Heart

            o
              | Just s <- v
              , shouldEnd <- standardOutro + s + heartNormal
              , time > shouldEnd = newCanvas i (setHeartEndTime (shouldEnd + heartEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ ( Tuple (setHeartBegTime i.eventStart acc)
                        $ filled
                            ( fillColor
                                (whiteRGBA ((time - i.eventStart) / standardIntro))
                            )
                            ( circle
                                (w / 2.0)
                                (h / 2.0)
                                cw
                            )
                    )
              | v
                  == Nothing
                  && dAcc
                      (sqToRect (w / 2.0) (h / 2.0) cw) = nextHeart acc i Just time
              | otherwise =
                pure
                  $ ( Tuple (maybe acc (flip setHeartStartTime acc) v)
                        $ ( filled
                              ( fillColor
                                  ( whiteRGBA
                                      ( case v of
                                          Nothing -> 1.0
                                          Just n' -> max 0.0 (min 1.0 (calcSlope (n' + heartNormal) 1.0 (standardOutro + n' + heartNormal) 0.0 time))
                                      )
                                  )
                              )
                              ( circle
                                  (w / 2.0)
                                  (h / 2.0)
                                  (cw * (1.0 + 0.08 * heartbeat 0.2 0.3 0.2 0.25 0.5 (maybe 0.0 (time - _) v)))
                              )
                          )
                    )
          in
            o
        Triangle v ->
          let
            sqv = sequence v

            top = V.index v d0

            left = V.index v d1

            right = V.index v d2

            cw = (min w h) / 16.0

            twoCw = 2.0 * cw

            nextTriangle = nextObj Triangle

            o
              | Just s <- sqv
              , shouldEnd <- standardOutro + foldl max 0.0 s
              , time > shouldEnd = newCanvas i (setTriangleEndTime (shouldEnd + triangleEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ ( Tuple (setTriangleBegTime i.eventStart acc)
                        $ fold
                            ( map
                                ( \pd ->
                                    let
                                      pdpi = pd * pi
                                    in
                                      filled
                                        ( fillColor
                                            (whiteRGBA ((time - i.eventStart) / standardIntro))
                                        )
                                        ( circle
                                            (sinp w pdpi)
                                            (cosp h pdpi)
                                            cw
                                        )
                                )
                                [ 0.0, 2.0 / 3.0, 4.0 / 3.0 ]
                            )
                    )
              | top
                  == Nothing
                  && dAcc
                      (sqToRect (sinp w (0.0 * pi)) (cosp h (0.0 * pi)) cw) = nextTriangle acc i (\t -> ((Just t) +> left +> right +> empty)) time
              | left
                  == Nothing
                  && dAcc
                      (sqToRect (sinp w (2.0 * pi / 3.0)) (cosp h (2.0 * pi / 3.0)) cw) = nextTriangle acc i (\t -> (top +> (Just t) +> right +> empty)) time
              | right
                  == Nothing
                  && dAcc
                      (sqToRect (sinp w (4.0 * pi / 3.0)) (cosp h (4.0 * pi / 3.0)) cw) = nextTriangle acc i (\t -> (top +> left +> (Just t) +> empty)) time
              | otherwise =
                pure
                  $ ( Tuple (setTriangleVector v acc)
                        $ fold
                            ( map
                                ( \(Tuple pd (Tuple bt n)) ->
                                    let
                                      pdpi = (pd + maybe 0.0 (\s -> (time - foldl max 0.0 s) `pow` 1.6) sqv) * pi

                                      mif = timeToMusicalInfo (time - quaver)

                                      r
                                        | mif.beat - bt < 0.3 = cw
                                        | mif.beat - bt < 0.5 = cw * (1.0 + calcSlope 0.3 0.0 0.5 0.3 (mif.beat - bt))
                                        | mif.beat - bt < 1.0 = cw * (1.0 + calcSlope 0.5 0.3 1.0 0.0 (mif.beat - bt))
                                        | otherwise = cw
                                    in
                                      filled
                                        ( fillColor
                                            ( whiteRGBA
                                                ( case n of
                                                    Nothing -> 1.0
                                                    Just n' -> maybe' (\_ -> max 0.4 (1.0 - (0.6 * (time - n') / standardPress))) (\s -> let mx = foldl max 0.0 s in calcSlope mx (if n' == mx then 1.0 else 0.4) (mx + standardOutro) 0.0 time) sqv
                                                )
                                            )
                                        )
                                        ( circle
                                            (sinp w pdpi)
                                            (cosp h pdpi)
                                            r
                                        )
                                )
                                [ Tuple 0.0 (Tuple 0.0 top)
                                , Tuple (2.0 / 3.0) (Tuple 2.0 left)
                                , Tuple (4.0 / 3.0) (Tuple 1.0 right)
                                ]
                            )
                    )
          in
            o
        Square v ->
          let
            sqv = sequence v

            c1 = 0.2

            c2 = 0.8

            topLeft = V.index v d0

            topRight = V.index v d1

            bottomLeft = V.index v d2

            bottomRight = V.index v d3

            cw = (min w h) / 17.0

            twoCw = 2.0 * cw

            nextSquare = nextObj Square

            o
              | Just s <- sqv
              , shouldEnd <- standardOutro + squareTravel + foldl max 0.0 s
              , time > shouldEnd = newCanvas i (setSquareEndTime (shouldEnd + squareEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ ( Tuple (setSquareBegTime i.eventStart acc)
                        $ fold
                            ( map
                                ( \{ x, y } ->
                                    filled (fillColor (whiteRGBA ((time - i.eventStart) / standardIntro)))
                                      (circle (x * w) (y * h) cw)
                                )
                                [ { x: c1, y: c1 }, { x: c2, y: c1 }, { x: c1, y: c2 }, { x: c2, y: c2 } ]
                            )
                    )
              | topLeft
                  == Nothing
                  && dAcc
                      (sqToRect (c1 * w) (c1 * h) cw) = nextSquare acc i (\t -> ((Just t) +> topRight +> bottomLeft +> bottomRight +> empty)) time
              | topRight
                  == Nothing
                  && dAcc
                      (sqToRect (c2 * w) (c1 * h) cw) = nextSquare acc i (\t -> (topLeft +> (Just t) +> bottomLeft +> bottomRight +> empty)) time
              | bottomLeft
                  == Nothing
                  && dAcc
                      (sqToRect (c1 * w) (c2 * h) cw) = nextSquare acc i (\t -> (topLeft +> topRight +> (Just t) +> bottomRight +> empty)) time
              | bottomRight
                  == Nothing
                  && dAcc
                      (sqToRect (c2 * w) (c2 * h) cw) = nextSquare acc i (\t -> (topLeft +> topRight +> bottomLeft +> (Just t) +> empty)) time
              | otherwise =
                pure
                  $ ( Tuple (setSquareVector v acc)
                        $ fold
                            ( map
                                ( \(Tuple { x0, y0, x1, y1 } n) ->
                                    let
                                      sqvT = maybe 0.0 (\s -> (time - foldl max 0.0 s)) sqv

                                      r = case n of
                                        Nothing -> cw
                                        Just n' -> pressEffect cw standardPress (time - n')
                                    in
                                      filled
                                        ( fillColor
                                            ( whiteRGBA
                                                ( case n of
                                                    Nothing -> 1.0
                                                    Just n' -> maybe' (\_ -> max 0.4 (1.0 - (0.6 * (time - n') / standardPress))) (\s -> let mx = foldl max 0.0 s in calcSlope mx (if n' == mx then 1.0 else 0.4) (mx + standardOutro) 0.0 time) sqv
                                                )
                                            )
                                        )
                                        ( circle
                                            ( case n of
                                                Nothing -> x0 * w
                                                Just n' -> w * (if (sqvT < squareTravel) then (calcSlope n' x0 (n' + squareTravel) 0.5 (min (n' + squareTravel) time)) else (calcSlope squareTravel (0.5) (standardOutro + squareTravel) x1 sqvT))
                                            )
                                            ( case n of
                                                Nothing -> y0 * h
                                                Just n' -> h * (if (sqvT < squareTravel) then (calcSlope n' y0 (n' + squareTravel) 0.5 (min (n' + squareTravel) time)) else (calcSlope squareTravel (0.5) (standardOutro + squareTravel) y1 sqvT))
                                            )
                                            r
                                        )
                                )
                                [ Tuple { x0: c1, y0: c1, x1: 1.1, y1: 1.1 } topLeft
                                , Tuple { x0: c2, y0: c1, x1: -0.1, y1: 1.1 } topRight
                                , Tuple { x0: c1, y0: c2, x1: 1.1, y1: -0.1 } bottomLeft
                                , Tuple { x0: c2, y0: c2, x1: -0.1, y1: -0.1 } bottomRight
                                ]
                            )
                    )
          in
            o
        Gears v ->
          let
            sqv = sequence v

            gear0 = V.index v d0

            gear1 = V.index v d1

            gear2 = V.index v d2

            gear3 = V.index v d3

            cw = (min w h) / 17.0

            cw0 = cw

            cw1 = cw0 + (w * 0.05)

            cw2 = cw1 + (w * 0.05)

            cw3 = cw2 + (w * 0.05)

            cg0 = 0.0

            cg1 = 0.3 * pi

            cg2 = 0.9 * pi

            cg3 = 1.8 * pi

            nextGear = nextObj Gears

            dArc cc =
              acc.initiatedClick
                && ( maybe false
                      ( \{ x, y } ->
                          if x == 0.0 && y == 0.0 then
                            false
                          else
                            ( let
                                hyp = pythag (x - w / 2.0) (y - h / 2.0)

                                lb = cc - (w * 0.025)

                                ub = cc + (w * 0.025)
                              in
                                hyp >= lb && hyp <= ub
                            )
                      )
                      acc.mousePosition
                  )

            gear2arc opq mcw gp =
              outlined (outlineColor (whiteRGBA opq) <> lineWidth (w * 0.05))
                (arc (w / 2.0) (h / 2.0) gp (gp + (calcSlope 0.0 1.0 w 2.0 mcw * pi)) mcw)

            o
              | Just s <- sqv
              , shouldEnd <- standardOutro + gearStay + foldl max 0.0 s
              , time > shouldEnd = newCanvas i (setGearsEndTime (shouldEnd + gearsEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ ( Tuple (setGearsBegTime i.eventStart acc)
                        $ fold
                            ( map
                                (\(Tuple mcw gs) -> gear2arc ((time - i.eventStart) / standardIntro) mcw gs)
                                [ Tuple cw0 cg0, Tuple cw1 cg1, Tuple cw2 cg2, Tuple cw3 cg3 ]
                            )
                    )
              | gear0
                  == Nothing
                  && dArc cw0 = nextGear acc i (\t -> ((Just t) +> gear1 +> gear2 +> gear3 +> empty)) time
              | gear1
                  == Nothing
                  && dArc cw1 = nextGear acc i (\t -> (gear0 +> (Just t) +> gear2 +> gear3 +> empty)) time
              | gear2
                  == Nothing
                  && dArc cw2 = nextGear acc i (\t -> (gear0 +> gear1 +> (Just t) +> gear3 +> empty)) time
              | gear3
                  == Nothing
                  && dArc cw3 = nextGear acc i (\t -> (gear0 +> gear1 +> gear2 +> (Just t) +> empty)) time
              | otherwise =
                pure
                  $ ( Tuple (setGearsVector v acc)
                        $ fold
                            ( map
                                ( \(Tuple (Tuple mcw gp) (Tuple gd n)) ->
                                    let
                                      nowT = maybe 0.0 (\s -> (time - foldl max 0.0 s)) sqv
                                    in
                                      gear2arc (maybe 1.0 (\s -> let mx = foldl max 0.0 s in (max 0.0 (min 1.0 (calcSlope (mx + gearStay) 1.0 (mx + gearStay + standardOutro) 0.0 time)))) sqv) mcw
                                        ( case n of
                                            Nothing -> gp
                                            Just n' -> (if gd then (+) else (-)) gp (gearSpinF (time - n'))
                                        )
                                )
                                [ Tuple (Tuple cw0 cg0) (Tuple gearDir0 gear0)
                                , Tuple (Tuple cw1 cg1) (Tuple gearDir1 gear1)
                                , Tuple (Tuple cw2 cg2) (Tuple gearDir2 gear2)
                                , Tuple (Tuple cw3 cg3) (Tuple gearDir3 gear3)
                                ]
                            )
                    )
          in
            o
        Motion prevT lr ->
          let
            cw = (min w h) / 16.0

            xp = eix w acc lr

            yp = eiy h acc lr

            textNormal = 1.0

            smp = setMotionPoints w h

            instr
              | time < i.eventStart + standardIntro + textNormal + standardOutro =
                let
                  opacity
                    | time < i.eventStart + standardIntro = calcSlope i.eventStart 0.0 (i.eventStart + standardIntro) 1.0 time
                    | time < i.eventStart + standardIntro + textNormal = 1.0
                    | time < i.eventStart + standardIntro + textNormal + standardOutro = calcSlope (i.eventStart + standardIntro + textNormal) 1.0 (i.eventStart + standardIntro + textNormal + standardOutro) 0.0 time
                    | otherwise = 0.0
                in
                  text
                    (font sansSerif 16 italic)
                    (w * 0.31)
                    (h * 0.31)
                    (fillColor (whiteRGBA opacity))
                    "(move the circle)"
              | otherwise = mempty

            o
              | shouldEnd <- i.eventStart + standardIntro + motionNormal + standardOutro
              , time > shouldEnd = newCanvas i (setMotionEndTime (shouldEnd + motionEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ Tuple (setMotionBegTime i.eventStart acc)
                      ( instr
                          <> filled
                              (fillColor (whiteRGBA (min 1.0 $ (time - i.eventStart) / standardIntro)))
                              (circle xp yp cw)
                      )
              | otherwise = case lr of
                Left _ ->
                  if needsToFollow xp yp cw acc lr then
                    let
                      oss = { x: (mouseOrBust acc.mousePosition).x - xp, y: (mouseOrBust acc.mousePosition).y - yp }

                      newX = calibrateX acc oss

                      newY = calibrateY acc oss

                      npt = { x: newX, y: newY }

                      ppt = { x: xp, y: yp }

                      newPt = Right (Tuple npt oss)
                    in
                      motionMaker (Motion (Just ppt) newPt) newX newY cw (smp (Tuple ppt npt) acc) i instr time
                  else
                    let npt = { x: xp, y: yp } in motionMaker (Motion prevT lr) xp yp cw (smp (Tuple (fromMaybe npt prevT) npt) acc) i instr time
                Right (Tuple old offset) ->
                  if needsToStopFollowing acc lr then
                    let
                      newPt = (Left { x: xp / w, y: yp / h })

                      nxp = (eix w acc newPt)

                      nyp = (eiy h acc newPt)
                    in
                      motionMaker (Motion (Just old) newPt) nxp nyp cw (smp (Tuple old { x: nxp, y: nyp }) acc) i instr time
                  else
                    let npt = { x: xp, y: yp } in motionMaker (Motion (Just old) (Right (Tuple npt offset))) xp yp cw (smp (Tuple old npt) acc) i instr time
          in
            o
        Rise v ->
          let
            one = V.index v d0

            two = V.index v d1

            three = V.index v d2

            four = V.index v d3

            five = V.index v d4

            six = V.index v d5

            cw = (min w h) / 21.0

            nextRise = nextObj Rise

            twoCw = 2.0 * cw

            tillIntroEnd = i.eventStart + standardIntro

            tillNormal = tillIntroEnd + riseNormal

            normalizedTime = pnCurve 1.2 $ (time - tillIntroEnd) / riseNormal

            heightNow = h * (calcSlope 0.0 0.9 1.0 0.1 (min normalizedTime 1.0))

            o
              | shouldEnd <- tillNormal + standardOutro
              , time > shouldEnd = newCanvas i (setRiseEndTime (shouldEnd + riseEndTimeBleed) acc) time
              | time < tillIntroEnd =
                pure
                  $ Tuple (setRiseBegTime i.eventStart acc)
                      ( fold
                          ( map
                              ( \xp ->
                                  ( filled
                                      (fillColor (whiteRGBA (min 1.0 $ (time - i.eventStart) / standardIntro)))
                                      (circle (w * xp) (h * 0.9) cw)
                                  )
                              )
                              riseXP
                          )
                      )
              | isNothing one
                  && dAcc
                      (sqToRect (1.0 * w / 12.0) (heightNow) cw) = nextRise acc i (\t -> (Just heightNow) +> two +> three +> four +> five +> six +> empty) time
              | isNothing two
                  && dAcc
                      (sqToRect (3.0 * w / 12.0) (heightNow) cw) = nextRise acc i (\t -> one +> (Just heightNow) +> three +> four +> five +> six +> empty) time
              | isNothing three
                  && dAcc
                      (sqToRect (5.0 * w / 12.0) (heightNow) cw) = nextRise acc i (\t -> one +> two +> (Just heightNow) +> four +> five +> six +> empty) time
              | isNothing four
                  && dAcc
                      (sqToRect (7.0 * w / 12.0) (heightNow) cw) = nextRise acc i (\t -> one +> two +> three +> (Just heightNow) +> five +> six +> empty) time
              | isNothing five
                  && dAcc
                      (sqToRect (9.0 * w / 12.0) (heightNow) cw) = nextRise acc i (\t -> one +> two +> three +> four +> (Just heightNow) +> six +> empty) time
              | isNothing six
                  && dAcc
                      (sqToRect (11.0 * w / 12.0) (heightNow) cw) = nextRise acc i (\t -> one +> two +> three +> four +> five +> (Just heightNow) +> empty) time
              | otherwise =
                pure
                  $ Tuple (setRiseVector v acc)
                      ( fold
                          ( map
                              ( \(Tuple xp pegged) ->
                                  ( filled
                                      (fillColor (whiteRGBA (if time > tillNormal then calcSlope tillNormal 1.0 (tillNormal + standardOutro) 0.0 time else 1.0)))
                                      (circle (w * xp) (fromMaybe heightNow pegged) cw)
                                  )
                              )
                              (zip riseXP [ one, two, three, four, five, six ])
                          )
                      )
          in
            o
        Shrink v ->
          let
            shrinkToRect (Tuple xp yp) rr = sqToRect (xp * w) (yp * h) (rr * (min w h) / 2.0)

            one = V.index v d0

            two = V.index v d1

            three = V.index v d2

            four = V.index v d3

            five = V.index v d4

            six = V.index v d5

            o
              -- todo: 1.5 magic number, change - allows for shrink
              | shouldEnd <- i.eventStart + standardIntro + shrinkNormal + standardOutro
              , time > shouldEnd = newCanvas i (setShrinkEndTime (shouldEnd + shrinkEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ ( Tuple (setShrinkBegTime i.eventStart acc)
                        $ fold
                            ( map
                                ( \(Tuple (Tuple xp yp) rr) ->
                                    filled (fillColor (whiteRGBA ((time - i.eventStart) / standardIntro)))
                                      (circle (xp * w) (yp * h) (rr * (min w h) / 2.0))
                                )
                                (V.zip (shrinkOne +> shrinkTwo +> shrinkThree +> shrinkFour +> shrinkFive +> shrinkSix +> empty) shrinkStart)
                            )
                    )
              | doingAction acc
                  (shrinkToRect shrinkOne (V.index v d0)) = paintShrinks w h acc i targetsOne v time
              | doingAction acc
                  (shrinkToRect shrinkTwo (V.index v d1)) = paintShrinks w h acc i targetsTwo v time
              | doingAction acc
                  (shrinkToRect shrinkThree (V.index v d2)) = paintShrinks w h acc i targetsThree v time
              | doingAction acc
                  (shrinkToRect shrinkFour (V.index v d3)) = paintShrinks w h acc i targetsFour v time
              | doingAction acc
                  (shrinkToRect shrinkFive (V.index v d4)) = paintShrinks w h acc i targetsFive v time
              | doingAction acc
                  (shrinkToRect shrinkSix (V.index v d5)) = paintShrinks w h acc i targetsSix v time
              | otherwise = paintShrinks w h acc i targetsDefault v time
          in
            o
        Snow a ->
          let
            stTime = i.eventStart

            nowT = time - stTime

            terminus = h * 1.1

            yp = snowYp h nowT

            o
              | not
                  ( bFoldL
                      ( \v x -> case x of
                          Nothing -> yp (maybe 100.0 (\(SnowI _ b _) -> b) (A.index snows v)) < terminus
                          (Just _) -> false
                      )
                      a
                  ) = newCanvas i (setSnowEndTime (time + snowEndTimeBleed) acc) time
              | otherwise = snowRecurser i w h acc stTime time a
          in
            o
        Bells a ->
          let
            stTime = i.eventStart

            nowT = time - stTime

            o
              | shouldEnd <- i.eventStart + standardIntro + bellsNormal + standardOutro
              , time > shouldEnd = newCanvas i (setBellsEndTime (shouldEnd + bellsEndTimeBleed) acc) time
              | time < i.eventStart + standardIntro =
                pure
                  $ Tuple (setBellsBegTime (i.eventStart) acc)
                      ( makeBells w h (A.replicate 24 (Tuple 1.0 (min 1.0 $ (time - i.eventStart) / standardIntro)))
                      )
              | otherwise = bellsRecurser i w h acc stTime time a
          in
            o
        Large v ->
          let
            cw = h / 1.2

            cst = 0.0 - cw - 10.0

            ced = w + cw + 1.0

            spn = ced - cst

            cpos = calcSlope 0.0 cst 1.0 ced (pnCurve 1.2 $ (time - i.eventStart) / largeCrossing)

            newV = maybe v (\mp -> if acc.initiatedClick then (Tuple mp time) : v else v) acc.mousePosition

            o
              | shouldEnd <- i.eventStart + largeCrossing + standardOutro
              , time > shouldEnd = newCanvas i (setLargeEndTime (shouldEnd + largeEndTimeBleed) acc) time
              | otherwise =
                pure
                  $ Tuple
                      ( (setLargeList (map snd newV) (setLargeBegTime i.eventStart acc))
                          { activity =
                            SilentNightPlayer
                              ( i
                                  { playerEvents = [ Large newV ] <> drop 1 i.playerEvents
                                  }
                              )
                          }
                      )
                      (filled (fillColor $ rgb 255 255 255) (circle cpos (h / 2.0) cw) <> fold (map (\(Tuple { x, y } tm) -> filled (fillColor $ rgb 0 0 0) (circle (calcSlope tm x (tm + largeCrossing) (x + spn) time) y ((min w h) * (min 0.3 $ 0.03 * (time - tm))))) newV))
          in
            o

calcMainStarts :: Number -> Number
calcMainStarts t =
  let
    -- where the piece starts, as there is a full bar rest
    -- so for example, at q=72, t=2.5 will yield 0.0
    placeInGrid = t - (3.0 * crotchet)

    diffFrom12 = ((placeInGrid * tempo / 60.0) % 12.0)

    -- zoom out if we're a full measure behind
    hd = if diffFrom12 > 8.3 then 24.0 else 12.0

    -- ie 12.0 - 5.0 = 7.0 beats remaining
    beatsRemaining = hd - diffFrom12
  in
    t + (beatsRemaining * crotchet)

scene :: Interactions -> Array PlayerEvent -> SilentNightAccumulator -> CanvasInfo -> Number -> Behavior (AV D2 SilentNightAccumulator)
scene inter evts acc' ci'@(CanvasInfo ci) time = go <$> (interactionLog inter)
  where
  go p =
    AV
      ( Just
          ( speaker'
              ( gain_ "globalMasterFader" (1.0) $ toNel players
              )
          )
      )
      (Just cvs)
      vizAcc
    where
    inCoda = maybe false (\v -> time >= v + pieceInMeasures) acc'.mainStarts

    codizedActivity =
      if inCoda && not acc'.initiatedCoda then
        ( ( case _ of
              SilentNightPlayer i ->
                SilentNightPlayer
                  i
                    { playerEvents = A.take 1 i.playerEvents <> [ Heart Nothing ]
                    }
              x -> x
          )
            acc'.activity
        )
      else
        acc'.activity

    acc =
      acc'
        { mousePosition =
          ( \{ x, y } ->
              { x: x - ci.boundingClientRect.x, y: y - ci.boundingClientRect.y
              }
          )
            <$> p.referencePosition
        , initiatedClick = (_.id <$> head p.interactions) /= acc'.curClickId
        , inClick = p.nInteractions /= 0
        , curClickId = _.id <$> head p.interactions
        , initiatedCoda = inCoda
        , activity = codizedActivity
        , mainStarts =
          if acc'.mainStarts /= Nothing then
            acc'.mainStarts
          else
            calcMainStarts <$> acc'.introEnds
        }

    (Tuple vizAcc cvs) = runReader (makeCanvas acc time) { evts, w: ci.w, h: ci.h }

    -- _______________________ = spy "audioMarkers" vizAcc.audioMarkers

    players =
      runReader silentNight
        { initiatedCoda: vizAcc.initiatedCoda
        , mainStarts: vizAcc.mainStarts
        , audioMarkers: vizAcc.audioMarkers
        , time: time
        , musicalInfo: timeToMusicalInfo time
        , verseStarts: vizAcc.verseStarts
        }

baseSnows = L.fromFoldable $ A.replicate snowL Nothing :: List (Maybe Number)

baseBells = L.fromFoldable $ A.replicate 24 Nil :: List (List Number)

allPlayerEvent =
  [ Triangle (fill (const Nothing))
  , Square (fill (const Nothing))
  , Motion Nothing (Left { x: 0.18, y: 0.18 })
  , Rise (fill (const Nothing))
  , Large Nil
  , Bells baseBells
  , Gears (fill (const Nothing))
  , Shrink shrinkStart
  , Snow baseSnows
  ] ::
    Array PlayerEvent

shrinkStart = 0.1 +> 0.15 +> 0.6 +> 0.3 +> 0.35 +> 0.05 +> empty :: Vec D6 Number

acA = Intro :: Activity

acB = HarmChooser { step: Row1Animation { startsAt: 0.0 } } :: Activity

acx :: Array PlayerEvent -> Activity
acx a =
  SilentNightPlayer
    { verse: Verse1
    , verseOne: VersionOne
    , verseTwo: VersionTwo
    , verseThree: VersionThree
    , playerEvents: a
    , eventStart: 0.0
    }

acC = acx [ Triangle (fill $ const Nothing), Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

acD = acx [ Square (fill $ const Nothing), Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

acE = acx [ Motion Nothing (Left { x: 0.18, y: 0.18 }), Square (fill $ const Nothing) ] :: Activity

acF = acx [ Rise (fill $ const Nothing), Square (fill $ const Nothing) ] :: Activity

acG = acx [ Snow (L.fromFoldable $ A.replicate snowL Nothing), Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

acH = acx [ Shrink shrinkStart, Square (fill $ const Nothing) ] :: Activity

acI = acx [ Bells (L.fromFoldable $ A.replicate 24 Nil), Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

acJ = acx [ Large Nil, Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

acK = acx [ Gears (fill $ const Nothing), Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

acL = acx [ Heart Nothing, Motion Nothing (Left { x: 0.18, y: 0.18 }) ] :: Activity

shuffle :: forall a. Array a -> Effect (Array a)
shuffle x = go 0 x
  where
  l = A.length x

  go 100 a = pure a

  go z a = do
    rd <- random
    let
      idx = floor (toNumber l * rd)
    go (z + 1) (A.drop idx a <> A.take idx a)

main :: Klank' SilentNightAccumulator
main =
  klank
    { run =
      runInBrowser_ do
        inter <- getInteractivity
        (Milliseconds timeNow) <- map unInstant now
        evts' <- shuffle allPlayerEvent
        evts <-
          sequence
            $ A.mapWithIndex
                ( \idx i -> do
                    n <- random
                    pure [ NoEvent (if idx == 0 then n * 2.0 + 5.0 else n * 5.0 + 2.0), i ]
                )
                evts'
        pure $ scene inter (join evts)
    , accumulator =
      \res _ ->
        res
          { initiatedClick: false
          , curClickId: Nothing
          , mousePosition: Nothing
          , activity: Intro
          , inClick: false
          , initiatedCoda: false
          , mainStarts: Nothing
          , introEnds: Nothing
          , verseStarts: { one: Nothing, two: Nothing, three: Nothing }
          , audioMarkers: defaultAudioMarkers
          }
    , exporter = defaultExporter
    , periodicWaves =
      \ctx _ res rej -> do
        smooth <-
          makePeriodicWave ctx
            (0.5 +> 0.25 +> -0.1 +> 0.07 +> 0.1 +> empty)
            (0.2 +> 0.1 +> 0.01 +> -0.03 +> -0.1 +> empty)
        rich <-
          makePeriodicWave ctx
            (0.1 +> 0.3 +> -0.1 +> 0.1 +> 0.2 +> 0.05 +> 0.1 +> 0.01 +> empty)
            (0.3 +> -0.5 +> -0.4 +> -0.03 +> -0.15 +> -0.2 +> -0.05 +> -0.02 +> empty)
        res $ O.fromFoldable [ Tuple "smooth" smooth, Tuple "rich" rich ]
    -- All sounds from freesound.org are attributed to their authors.
    , buffers =
      makeBuffersKeepingCache
        $ [ Tuple "metronome-wb" "https://freesound.org/data/previews/53/53403_400592-lq.mp3"
          , Tuple "hb1" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/hb1.ogg"
          , Tuple "hb2" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/hb2.ogg"
          , Tuple "IntroLoopA" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/introC.mp3"
          , Tuple "IntroLoopB" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/introD.mp3"
          , Tuple "IntroLoopC" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/introA.mp3"
          , Tuple "IntroLoopD" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/introB.mp3"
          , Tuple "IntroLoopE" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/introE.mp3"
          , Tuple "v1t1" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v1t1.mp3"
          , Tuple "v1t2" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v1t2.mp3"
          , Tuple "v1t3" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v1t3.mp3"
          , Tuple "v1t4" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v1t4.mp3"
          , Tuple "v1t5" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v1t5.mp3"
          , Tuple "v1t6" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v1t6.mp3"
          , Tuple "v1t7" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v1t7.mp3"
          , Tuple "v1t8" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v1t8.mp3"
          , Tuple "v2t1" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v2t1.mp3"
          , Tuple "v2t2" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v2t2.mp3"
          , Tuple "v2t3" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v2t3.mp3"
          , Tuple "v2t4" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v2t4.mp3"
          , Tuple "v2t5" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v2t5.mp3"
          , Tuple "v2t6" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v2t6.mp3"
          , Tuple "v2t7" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v2t7.mp3"
          , Tuple "v2t8" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v2t8.mp3"
          , Tuple "v3t1" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v3t1.mp3"
          , Tuple "v3t2" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v3t2.mp3"
          , Tuple "v3t3" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v3t3.mp3"
          , Tuple "v3t4" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v3t4.mp3"
          , Tuple "v3t5" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v3t5.mp3"
          , Tuple "v3t6" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v3t6.mp3"
          , Tuple "v3t7" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v3t7.mp3"
          , Tuple "v3t8" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/v3t8.mp3"
          , Tuple "choiceBell" "https://freesound.org/data/previews/411/411089_5121236-hq.mp3"
          , Tuple "motion" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/motion.ogg"
          , Tuple "snow" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/snow.mp3"
          , Tuple "triangle" "https://freesound.org/data/previews/199/199822_3485902-hq.mp3"
          , Tuple "square1" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/square1.ogg"
          , Tuple "square2" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/square2.ogg"
          , Tuple "square3" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/square3.ogg"
          , Tuple "square4" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/square4.ogg"
          , Tuple "square5" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/square5.ogg"
          , Tuple "gearBowl" "https://klank-share.s3-eu-west-1.amazonaws.com/in-a-sentimental-mood/Samples/SingingBowls/Small---Perform-1.r.ogg"
          , Tuple "large-birds" "https://freesound.org/data/previews/387/387978_6221013-hq.mp3"
          , Tuple "large-market" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/129677_2355772-hq.mp3"
          , Tuple "large-chimes" "https://freesound.org/data/previews/136/136299_1645319-hq.mp3"
          , Tuple "large-synth" "https://freesound.org/data/previews/353/353501_5121236-hq.mp3" -- "Ambience, Wind Chimes, A.wav" by InspectorJ (www.jshaw.co.uk) of Freesound.org
          , Tuple "large-santa" "https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/santa.mp3"
          , Tuple "rise" "https://freesound.org/data/previews/110/110158_649468-hq.mp3"
          ]
        <> join (map (\x -> map (\i -> Tuple (x <> show i) ("https://klank-share.s3-eu-west-1.amazonaws.com/silent-night/Bells/" <> x <> show i <> ".ogg")) (A.range 0 47)) [ "m", "kg", "sb", "rb" ])
    }

newtype Interactions
  = Interactions
  { interactions :: Ref.Ref (InteractionOnsets)
  , nInteractions :: Ref.Ref Int
  , referencePosition :: Ref.Ref (Maybe Point)
  , dispose :: Effect Unit
  }

type InteractionOnsets
  = Array
      { id :: Int
      , x :: Number
      , y :: Number
      }

handleTE :: Int -> Ref.Ref (InteractionOnsets) -> Ref.Ref (Maybe Point) -> TouchEvent -> Effect Unit
handleTE i ref pr te = do
  let
    ts = changedTouches te
  let
    l = TL.length ts
  let
    tlist = map (\t -> { id: i, x: toNumber $ T.clientX t, y: toNumber $ T.clientY t }) (catMaybes $ map (\x -> TL.item x ts) (range 0 (l - 1)))
  Ref.write (map (\{ x, y } -> { x, y }) (head tlist)) pr
  void $ Ref.modify (\ipt -> tlist <> ipt) ref

handleME :: Int -> Ref.Ref (InteractionOnsets) -> Ref.Ref (Maybe Point) -> MouseEvent -> Effect Unit
handleME id ref pr me = do
  let
    x = toNumber $ ME.clientX me
  let
    y = toNumber $ ME.clientY me
  Ref.write (Just { x, y }) pr
  void $ Ref.modify (\ipt -> [ { id, x, y } ] <> ipt) ref

handleTM :: Ref.Ref (Maybe Point) -> TouchEvent -> Effect Unit
handleTM pr te = do
  let
    ts = changedTouches te
  let
    l = TL.length ts
  let
    tlist = map (\t -> { x: toNumber $ T.clientX t, y: toNumber $ T.clientY t }) (catMaybes $ map (\x -> TL.item x ts) (range 0 (l - 1)))
  Ref.write (map (\{ x, y } -> { x, y }) (head tlist)) pr

handleMM :: Ref.Ref (Maybe Point) -> MouseEvent -> Effect Unit
handleMM pr me = do
  let
    x = toNumber $ ME.clientX me
  let
    y = toNumber $ ME.clientY me
  Ref.write (Just { x, y }) pr

getInteractivity :: Effect Interactions
getInteractivity = do
  w <- window
  nav <- navigator w
  ua <- userAgent nav
  let
    mobile = isJust (indexOf (Pattern "iPhone") ua) || isJust (indexOf (Pattern "iPad") ua) || isJust (indexOf (Pattern "Android") ua)
  nInteractions <- Ref.new 0
  referencePosition <- Ref.new Nothing
  totalInteractions <- Ref.new 0
  interactions <- Ref.new []
  target <- toEventTarget <$> window
  touchStartListener <-
    eventListener \e -> do
      fromEvent e
        # traverse_ \me -> do
            void $ Ref.modify (_ + 1) nInteractions
            nt <- Ref.modify (_ + 1) totalInteractions
            handleTE nt interactions referencePosition me
  touchMoveListener <-
    eventListener \e -> do
      fromEvent e
        # traverse_ \me -> do
            handleTM referencePosition me
  touchEndListener <-
    eventListener \e -> do
      fromEvent e
        # traverse_ \me -> do
            void $ Ref.modify (_ - 1) nInteractions
  mouseDownListener <-
    eventListener \e -> do
      ME.fromEvent e
        # traverse_ \me -> do
            void $ Ref.modify (_ + 1) nInteractions
            nt <- Ref.modify (_ + 1) totalInteractions
            handleME nt interactions referencePosition me
  mouseMoveListener <-
    eventListener \e -> do
      ME.fromEvent e
        # traverse_ \me -> do
            handleMM referencePosition me
  mouseUpListener <-
    eventListener \e -> do
      ME.fromEvent e
        # traverse_ \me -> do
            void $ Ref.modify (_ - 1) nInteractions
  if mobile then do
    addEventListener (wrap "touchstart") touchStartListener false target
    addEventListener (wrap "touchmove") touchMoveListener false target
    addEventListener (wrap "touchend") touchEndListener false target
  else do
    addEventListener (wrap "mousedown") mouseDownListener false target
    addEventListener (wrap "mousemove") mouseMoveListener false target
    addEventListener (wrap "mouseup") mouseUpListener false target
  let
    dispose =
      if mobile then do
        removeEventListener (wrap "touchstart") touchStartListener false target
        removeEventListener (wrap "touchmove") touchMoveListener false target
        removeEventListener (wrap "touchend") touchEndListener false target
      else do
        removeEventListener (wrap "mousedown") mouseDownListener false target
        removeEventListener (wrap "mousemove") mouseMoveListener false target
        removeEventListener (wrap "mouseup") mouseUpListener false target
  pure (Interactions { interactions, referencePosition, nInteractions, dispose })

withInteractions ::
  forall a.
  Interactions ->
  Event a ->
  Event { value :: a, interactions :: InteractionOnsets, nInteractions :: Int, referencePosition :: Maybe Point }
withInteractions (Interactions { interactions, nInteractions, referencePosition }) e =
  makeEvent \k ->
    e
      `subscribe`
        \value -> do
          interactionsValue <- Ref.read interactions
          nInteractionsValue <- Ref.read nInteractions
          referencePositionValue <- Ref.read referencePosition
          k { value, interactions: interactionsValue, nInteractions: nInteractionsValue, referencePosition: referencePositionValue }

interactionLog :: Interactions -> Behavior ({ interactions :: InteractionOnsets, nInteractions :: Int, referencePosition :: Maybe Point })
interactionLog m = behavior \e -> map (\{ value, interactions, nInteractions, referencePosition } -> value { interactions, nInteractions, referencePosition }) (withInteractions m e)

-- print(",".join(["{x:"+str(random.random())+",y:"+str(random.random())+",f: \\x -> "+str(random.random()*0.9+0.1)+" * sin ("+str(random.random()*0.6 + 0.1)+" * (x + "+str(random.random())+") * pi) }" for x in range(100)]))
starDs :: Array { x :: Number, y :: Number, f :: Number -> Number }
starDs = [ { x: 0.7998979153795341, y: 0.08385139594872981, f: \x -> 0.7657069009122556 * sin (0.12048910397521838 * (x + 0.9235025221582407) * pi) }, { x: 0.1889061340397945, y: 0.41662576058498235, f: \x -> 0.1415196387959073 * sin (0.5683357566654806 * (x + 0.4140936479451277) * pi) }, { x: 0.6415434619211883, y: 0.5708146975512279, f: \x -> 0.11577248196435784 * sin (0.29554293868038256 * (x + 0.3836381290510813) * pi) }, { x: 0.46408624003822874, y: 0.5352411436249525, f: \x -> 0.27899259750022887 * sin (0.6168518732109306 * (x + 0.377444452261138) * pi) }, { x: 0.9372148362501318, y: 0.3739296461256748, f: \x -> 0.671489098410653 * sin (0.4822310763852671 * (x + 0.3908243855120592) * pi) }, { x: 0.9677579259565381, y: 0.07151980503063837, f: \x -> 0.9752455547563101 * sin (0.6051350059573541 * (x + 0.46951319522277657) * pi) }, { x: 0.5491926546092344, y: 0.07374039190919535, f: \x -> 0.33536560976485197 * sin (0.53634515525111 * (x + 0.3411527658666662) * pi) }, { x: 0.522532549928989, y: 0.24000747216531726, f: \x -> 0.6849413502333674 * sin (0.25061020850894333 * (x + 0.2704271222503388) * pi) }, { x: 0.676735264877141, y: 0.154014527584188, f: \x -> 0.643492816957883 * sin (0.4999621556849063 * (x + 0.23972440926252403) * pi) }, { x: 0.022005599332260095, y: 0.4517846398419565, f: \x -> 0.9780033023620383 * sin (0.5690010434724488 * (x + 0.4700260516669268) * pi) }, { x: 0.6623321309130382, y: 0.1538961699400414, f: \x -> 0.39547598751018875 * sin (0.35752515144399377 * (x + 0.8345066395190842) * pi) }, { x: 0.07008140643788907, y: 0.5088425430429082, f: \x -> 0.938078599349549 * sin (0.21111028367327936 * (x + 0.8390425040791938) * pi) }, { x: 0.8183208236713514, y: 0.12015800392666187, f: \x -> 0.8959988138316818 * sin (0.6783042097542152 * (x + 0.003916202043623196) * pi) }, { x: 0.6567155163250299, y: 0.8911844616174074, f: \x -> 0.23654036104506207 * sin (0.2682238556417563 * (x + 0.4334644233731685) * pi) }, { x: 0.6635142395130955, y: 0.16220416064115362, f: \x -> 0.8813181743932739 * sin (0.545982341474434 * (x + 0.8337268747684451) * pi) }, { x: 0.27523028020466733, y: 0.01542831693086355, f: \x -> 0.8125688271194221 * sin (0.11256935190258262 * (x + 0.6476702955558028) * pi) }, { x: 0.5999468031487667, y: 0.9508123847205505, f: \x -> 0.12455506698851554 * sin (0.16642138506387108 * (x + 0.9842802977680329) * pi) }, { x: 0.5264600473652116, y: 0.2521314595090074, f: \x -> 0.6004655754729078 * sin (0.5488467917947338 * (x + 0.4100251478253423) * pi) }, { x: 0.2368072536753597, y: 0.3972845965548708, f: \x -> 0.7391259416146705 * sin (0.43050261122448596 * (x + 0.3697052483399993) * pi) }, { x: 0.17679605291326028, y: 0.6268540032189557, f: \x -> 0.6546348882779175 * sin (0.6721869642094553 * (x + 0.24128601888888124) * pi) }, { x: 0.8743480466826442, y: 0.0840467624458574, f: \x -> 0.48785103770632665 * sin (0.3933065653420563 * (x + 0.7481526400188201) * pi) }, { x: 0.061704426811517776, y: 0.34095357496002165, f: \x -> 0.7952378622812539 * sin (0.2073614887237352 * (x + 0.07236472200581767) * pi) }, { x: 0.046728095535261915, y: 0.3557906691124455, f: \x -> 0.8471468676835398 * sin (0.6936312563009017 * (x + 0.8884226981678148) * pi) }, { x: 0.9420065262296728, y: 0.46984187619709383, f: \x -> 0.9148056281198275 * sin (0.21228333966864504 * (x + 0.12171006947395191) * pi) }, { x: 0.22828963814693148, y: 0.5936010890996245, f: \x -> 0.5869924335309348 * sin (0.6760951901932156 * (x + 0.42581741442330057) * pi) }, { x: 0.9448422412741705, y: 0.847273328594833, f: \x -> 0.1444443072829259 * sin (0.564055969475472 * (x + 0.23828946968648346) * pi) }, { x: 0.026017619239415812, y: 0.38784732596558724, f: \x -> 0.40568046115459055 * sin (0.5429357146134501 * (x + 0.17243981005583087) * pi) }, { x: 0.8600314612590836, y: 0.16872127332155784, f: \x -> 0.5243336361413855 * sin (0.5770135444505058 * (x + 0.5096273590873813) * pi) }, { x: 0.6380405298096601, y: 0.47804272716920826, f: \x -> 0.9577493093614781 * sin (0.2636125952601193 * (x + 0.22133762993410544) * pi) }, { x: 0.9623176544984414, y: 0.5731719748334171, f: \x -> 0.8510815994212508 * sin (0.23713905867729537 * (x + 0.9659410292888134) * pi) }, { x: 0.2715621864828037, y: 0.5746086436300603, f: \x -> 0.2476824596259333 * sin (0.6883522844630905 * (x + 0.8934386328562979) * pi) }, { x: 0.9163545335851067, y: 0.5876324876520401, f: \x -> 0.29767964201568 * sin (0.32407987185183196 * (x + 0.5624478725260176) * pi) }, { x: 0.5121105811687144, y: 0.5976398842996643, f: \x -> 0.9487706274379716 * sin (0.3782745410559547 * (x + 0.005825894962646516) * pi) }, { x: 0.42866822912910885, y: 0.6830883083393303, f: \x -> 0.22875670760621292 * sin (0.1725422234511803 * (x + 0.4988007813609098) * pi) }, { x: 0.581878431057003, y: 0.9246881434525345, f: \x -> 0.5571826049082016 * sin (0.664291356515955 * (x + 0.4928719048473047) * pi) }, { x: 0.3356527164199967, y: 0.29328180629071976, f: \x -> 0.845038335641492 * sin (0.695613646890855 * (x + 0.9700923098200159) * pi) }, { x: 0.10749775381328286, y: 0.5554202833939391, f: \x -> 0.739285784528385 * sin (0.6469822932763877 * (x + 0.17466536383933384) * pi) }, { x: 0.8950320550823506, y: 0.6678599725651887, f: \x -> 0.503171785472261 * sin (0.28611277693258097 * (x + 0.6473420586463666) * pi) }, { x: 0.1443247940739063, y: 0.010924436297578888, f: \x -> 0.6986998672144634 * sin (0.17255867274134484 * (x + 0.12993132146898623) * pi) }, { x: 0.2791503707404528, y: 0.06312099128646453, f: \x -> 0.36429082463506834 * sin (0.34196259679059626 * (x + 0.6979922700561009) * pi) }, { x: 0.9285299556309622, y: 0.10161382562297627, f: \x -> 0.8084105943491684 * sin (0.3347280804116324 * (x + 0.649834380138763) * pi) }, { x: 0.5087841904358481, y: 0.5148341219339472, f: \x -> 0.12821142080843256 * sin (0.6436397049542052 * (x + 0.49377239663519) * pi) }, { x: 0.5767279353705533, y: 0.5017259429620036, f: \x -> 0.7074152315694675 * sin (0.24359373059476075 * (x + 0.3640710968785963) * pi) }, { x: 0.3572829307534393, y: 0.07930845199331438, f: \x -> 0.12272185939340002 * sin (0.21496787160199268 * (x + 0.8232597611434008) * pi) }, { x: 0.3045478568273394, y: 0.15189984836967618, f: \x -> 0.44456300688412687 * sin (0.5905856604494417 * (x + 0.6019971776570228) * pi) }, { x: 0.43551591756677743, y: 0.7888726012495775, f: \x -> 0.39358983003016523 * sin (0.46177449026369366 * (x + 0.6330979653144985) * pi) }, { x: 0.8996122943728967, y: 0.4193294174077634, f: \x -> 0.20922101207535942 * sin (0.2505708338272897 * (x + 0.6669132311420939) * pi) }, { x: 0.006809687407536957, y: 0.2591751351787317, f: \x -> 0.4990002170158462 * sin (0.16376093348549342 * (x + 0.9917153566334772) * pi) }, { x: 0.28360533801009014, y: 0.8965273384199693, f: \x -> 0.832548216777215 * sin (0.6968597343485691 * (x + 0.6522250227849495) * pi) }, { x: 0.11768473063078866, y: 0.47431612674759016, f: \x -> 0.3069972229715307 * sin (0.35402930954677736 * (x + 0.2598129062355331) * pi) }, { x: 0.9605409025944893, y: 0.9210189781644056, f: \x -> 0.13767653252020387 * sin (0.645397983120965 * (x + 0.20332445324936355) * pi) }, { x: 0.5802648295815812, y: 0.32188272064836765, f: \x -> 0.7926623083133738 * sin (0.5024237429565865 * (x + 0.43358317968813154) * pi) }, { x: 0.460669273896289, y: 0.826858493037314, f: \x -> 0.6875062564366267 * sin (0.32204424860440284 * (x + 0.6846140018360137) * pi) }, { x: 0.49649673809381545, y: 0.5368631031674957, f: \x -> 0.45981852239312815 * sin (0.4007905113743796 * (x + 0.3419384588612011) * pi) }, { x: 0.014953390603937744, y: 0.7378416524667052, f: \x -> 0.6140466078126797 * sin (0.4870594133670276 * (x + 0.5499076425091735) * pi) }, { x: 0.9651492489337455, y: 0.034190311101410464, f: \x -> 0.680725250861211 * sin (0.1404764045758468 * (x + 0.027344375850516744) * pi) }, { x: 0.3489646579749772, y: 0.45094294411398583, f: \x -> 0.4762308773272914 * sin (0.47885316186142957 * (x + 0.4348305860227125) * pi) }, { x: 0.721516436343126, y: 0.30602540798118605, f: \x -> 0.9539093982726269 * sin (0.12257732836191289 * (x + 0.3236116142830592) * pi) }, { x: 0.0361505728383702, y: 0.22671487550392044, f: \x -> 0.5470863801997212 * sin (0.2267991542971995 * (x + 0.9817698978873135) * pi) }, { x: 0.383638361887656, y: 0.3918972451520907, f: \x -> 0.34106319921048894 * sin (0.6698729229823568 * (x + 0.9500835732982084) * pi) }, { x: 0.19432877382630098, y: 0.9522640114916134, f: \x -> 0.5234030615472549 * sin (0.29322018355964974 * (x + 0.3719893915176793) * pi) }, { x: 0.46657705708863173, y: 0.12174792367473619, f: \x -> 0.7047731530960449 * sin (0.39527154230290673 * (x + 0.5936262233984391) * pi) }, { x: 0.21703176126522317, y: 0.8930891192173489, f: \x -> 0.33641264539117166 * sin (0.4493531696446884 * (x + 0.2517156629417422) * pi) }, { x: 0.9174044923141177, y: 0.8817466776391724, f: \x -> 0.8454621762175387 * sin (0.115257536467252 * (x + 0.03469723233181332) * pi) }, { x: 0.04039986415756336, y: 0.15271145439316458, f: \x -> 0.9490449699465466 * sin (0.6750180669000483 * (x + 0.23431970196414276) * pi) }, { x: 0.5659642215562013, y: 0.33905297640111876, f: \x -> 0.5954679109024937 * sin (0.32255562390725917 * (x + 0.4103045663404872) * pi) }, { x: 0.5932269683893593, y: 0.04137646988081334, f: \x -> 0.43123850944012776 * sin (0.3503950236984895 * (x + 0.8751854149669083) * pi) }, { x: 0.2600532049644848, y: 0.9512335323594845, f: \x -> 0.1330852815646522 * sin (0.11685915493811834 * (x + 0.8032567255861771) * pi) }, { x: 0.750596919302173, y: 0.5660647811529884, f: \x -> 0.12252811048084478 * sin (0.4288900646165352 * (x + 0.8220020230866586) * pi) }, { x: 0.7058481499412821, y: 0.33577510035321756, f: \x -> 0.9309011132534613 * sin (0.10807387415279282 * (x + 0.5466506710634887) * pi) }, { x: 0.7560511451536488, y: 0.06454735707577242, f: \x -> 0.43828083133250995 * sin (0.4445668204142387 * (x + 0.054743827468044626) * pi) }, { x: 0.6481088042227873, y: 0.6090098373024916, f: \x -> 0.14389170336038942 * sin (0.33867071677250293 * (x + 0.4469693618232342) * pi) }, { x: 0.36024818355657573, y: 0.44495171974439407, f: \x -> 0.9902656439613039 * sin (0.3127988834819777 * (x + 0.9987847933230615) * pi) }, { x: 0.4948827944750618, y: 0.6429648291847946, f: \x -> 0.4301864871375427 * sin (0.6723415663046644 * (x + 0.09093914204530151) * pi) }, { x: 0.19745107701800912, y: 0.7921651204622543, f: \x -> 0.9097728915806184 * sin (0.1963810981662855 * (x + 0.7245090758938467) * pi) }, { x: 0.11912897419443358, y: 0.03254896045113975, f: \x -> 0.22994027602254857 * sin (0.6939778711871162 * (x + 0.7388155499569684) * pi) }, { x: 0.24415494669411641, y: 0.472570097337776, f: \x -> 0.22902259190972618 * sin (0.615227866357919 * (x + 0.9890480993112479) * pi) }, { x: 0.9203841664465398, y: 0.6208633185102296, f: \x -> 0.3143331078045957 * sin (0.6815435814010501 * (x + 0.057924771636933325) * pi) }, { x: 0.9811499004861685, y: 0.5022262463844889, f: \x -> 0.21672054629593676 * sin (0.6210527552028506 * (x + 0.7557243610589263) * pi) }, { x: 0.9677634098982452, y: 0.3901345287041713, f: \x -> 0.43036366949289895 * sin (0.4233216192964807 * (x + 0.31944166044882005) * pi) }, { x: 0.5786073690858976, y: 0.586371361406231, f: \x -> 0.13646859604175726 * sin (0.12072088512536519 * (x + 0.5010522848830792) * pi) }, { x: 0.0012499448415448366, y: 0.12587572124009438, f: \x -> 0.18469053046531844 * sin (0.42390239395966556 * (x + 0.1531961573744779) * pi) }, { x: 0.16895243179895392, y: 0.5277320979707424, f: \x -> 0.865882371139905 * sin (0.5691280576641032 * (x + 0.3324058336640292) * pi) }, { x: 0.801460966627075, y: 0.45910383316395653, f: \x -> 0.6598164595121057 * sin (0.5260478420336041 * (x + 0.6697376194661933) * pi) }, { x: 0.8521339389706153, y: 0.3626348770849186, f: \x -> 0.7924312976020662 * sin (0.2991814837291323 * (x + 0.909971642527428) * pi) }, { x: 0.674812734540027, y: 0.7277770593790434, f: \x -> 0.6762131320540856 * sin (0.5681512855133772 * (x + 0.5556219498125824) * pi) }, { x: 0.6491557825332536, y: 0.07308319042126221, f: \x -> 0.4735695380926135 * sin (0.40780536241728604 * (x + 0.5349601344729933) * pi) }, { x: 0.42464225237190634, y: 0.7069760605800132, f: \x -> 0.535850587398882 * sin (0.2955149383605018 * (x + 0.20791069400385453) * pi) }, { x: 0.20088096344665818, y: 0.09158856466883636, f: \x -> 0.483227501295338 * sin (0.6709549705061462 * (x + 0.4560022670904379) * pi) }, { x: 0.3001051588250363, y: 0.33328222376853534, f: \x -> 0.9773099994432405 * sin (0.2698851893399844 * (x + 0.004287367099682582) * pi) }, { x: 0.7865828579399956, y: 0.49798726608825594, f: \x -> 0.4015876199547852 * sin (0.5042292671040394 * (x + 0.8001839080045724) * pi) }, { x: 0.5559852141603664, y: 0.7666693336871541, f: \x -> 0.647911241286775 * sin (0.3930000983877372 * (x + 0.28698956153301447) * pi) }, { x: 0.34500972739493296, y: 0.5973390313634365, f: \x -> 0.6587863792858866 * sin (0.46869827306319156 * (x + 0.15114144868234736) * pi) }, { x: 0.23374379075588625, y: 0.7583632900663713, f: \x -> 0.7400567456600466 * sin (0.29787747244782165 * (x + 0.6753439286421234) * pi) }, { x: 0.6873905620914865, y: 0.5451169574060583, f: \x -> 0.11367639049636262 * sin (0.3297740241276462 * (x + 0.020127227356116162) * pi) }, { x: 0.3355756682713278, y: 0.9359876560341573, f: \x -> 0.7549390556872413 * sin (0.1449320748833887 * (x + 0.739847533268351) * pi) }, { x: 0.6276724894939283, y: 0.5668762277782079, f: \x -> 0.2656702822304661 * sin (0.16571984171700352 * (x + 0.8723894908240698) * pi) }, { x: 0.22465866093491016, y: 0.7129778383867211, f: \x -> 0.3545569459686496 * sin (0.5742628607823999 * (x + 0.5172389520132499) * pi) }, { x: 0.8575345791452911, y: 0.47901213109080476, f: \x -> 0.40539337284147625 * sin (0.34845123795736366 * (x + 0.5940440256190052) * pi) }, { x: 0.020995471036586055, y: 0.33573817904636394, f: \x -> 0.5670171907199093 * sin (0.13614960442098947 * (x + 0.27164164393555656) * pi) } ]

star :: Number -> Number -> (Number -> Number) -> Number -> Number -> Number -> Drawing
star x y f w h t = let c = floor $ calcSlope (-1.0) 120.0 1.0 255.0 (f t) in filled (fillColor (rgb c c c)) (circle (x * w) (y * h) 1.0)

starFs = map (\{ x, y, f } -> star x y f) starDs :: Array (Number -> Number -> Number -> Drawing)

data SnowI
  = SnowI Number Number Number

-- print(",".join(["SnowI "+str(random.random())+" "+str(random.random())+" "+str(random.random()) for x in range(42)]))
snows = [ SnowI 0.1763482237244488 0.7843392558067448 0.8736775739009528, SnowI 0.28200324903345875 0.7616174886387548 0.9886386189118499, SnowI 0.5333998008487347 0.9930176360953382 0.4358035440368596, SnowI 0.6215201584313228 0.2579016465924222 0.9752643594074341, SnowI 0.866939186383011 0.8360805146799725 0.7958765209012253, SnowI 0.8316783439260418 0.37836656895524046 0.5791729169409586, SnowI 0.2733107241502992 0.3612653659521434 0.19231210814850852, SnowI 0.31027228153383757 0.13944530644247088 0.7550167721602923, SnowI 0.9998447705284339 0.6730718131664458 0.8426964150644125, SnowI 0.17929816108198704 0.790475448268706 0.0003747545515384587, SnowI 0.5146676245826576 0.9116200472758966 0.8531328483755889, SnowI 0.16350352611390262 0.035050938943344545 0.12000898554296346, SnowI 0.02235639770750919 0.2637379491356546 0.38653640592838456, SnowI 0.09638415930577127 0.5450761098431707 0.04520437463832483, SnowI 0.4892276041330552 0.3173833927642091 0.9635762922386145, SnowI 0.5579747466273458 0.4261821763227277 0.034829543965688825, SnowI 0.1383348346924944 0.8730898886001669 0.5821842719623467, SnowI 0.5417335843317096 0.5227917372193671 0.5841349868208985, SnowI 0.561091946480468 0.7226139875075132 0.3254593174473801, SnowI 0.18440849817668648 0.8842935817289816 0.23906878196524894, SnowI 0.8395874260044586 0.28184640063208755 0.14333272039170541, SnowI 0.6156612759042932 0.713960173022045 0.7480729332633452, SnowI 0.38173188767605626 0.7387808291538753 0.6278447076280427, SnowI 0.24589553416745413 0.5290708205531433 0.32196549155354526, SnowI 0.35871499328693446 0.7548104803495969 0.20947469621974146, SnowI 0.7788111970805636 0.5267905775598622 0.6587485955119049, SnowI 0.6335840127131971 0.22575848163364354 0.8774238179179625, SnowI 0.4775763671281781 0.23139336402021182 0.4714066657711262, SnowI 0.6117193254284164 0.5404145696505988 0.9423220409781308, SnowI 0.8393112405852664 0.5386134230858991 0.34356584701617143, SnowI 0.7273029953298517 0.9157883450982325 0.5161433407992876, SnowI 0.7523225195383978 0.9706116721435495 0.8485145712220996, SnowI 0.25565438866795875 0.4858444945461321 0.02409997710228562, SnowI 0.2482461461505997 0.06998419990506399 0.8023056575942396, SnowI 0.6447100246955894 0.23461445726865426 0.7437139284297501, SnowI 0.4037462586982459 0.8347403986979446 0.6252308698800807, SnowI 0.5777309693538369 0.7523862184546504 0.7963104491899832, SnowI 0.10765091503325408 0.9055179728124041 0.5411994836080661, SnowI 0.009800138222332277 0.2174946835643694 0.9471799584836776, SnowI 0.5718328748696805 0.1991607030882191 0.6324687034592706, SnowI 0.009331228956806159 0.8127670538006947 0.10168511813889736, SnowI 0.971185774284385 0.6019894835093992 0.5234232707786459 ] :: Array SnowI

snowL = A.length snows :: Int

snowList = L.fromFoldable snows :: List SnowI
