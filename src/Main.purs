module Klank.NatureBoy where

import Prelude
import Color (Color, rgb, rgba)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Parallel (parallel, sequential)
import Control.Promise (toAffE)
import Data.Array (catMaybes, drop, filter, fold, head, range, zip)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..), either, isLeft)
import Data.Foldable (class Foldable, foldl, traverse_)
import Data.Int (floor, toNumber)
import Data.Lens (_2, over)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe, maybe')
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (lcmap)
import Data.String (Pattern(..), indexOf)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Data.Typelevel.Num (class Nat, D10, D2, D24, D3, D4, D42, D5, D6, d0, d1, d2, d3, d4, d5)
import Data.Vec (Vec, empty, fill, (+>))
import Data.Vec as V
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, try)
import Effect.Exception (Error)
import Effect.Now (now)
import Effect.Random (random)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Audio (AV(..), AudioContext, AudioParameter, BrowserAudioBuffer, CanvasInfo(..), Instruction, decodeAudioDataFromUri, defaultExporter, evalPiecewise, gain_, runInBrowser_, speaker')
import FRP.Event (Event, makeEvent, subscribe)
import Foreign.Object as O
import Graphics.Canvas (Rectangle)
import Graphics.Drawing (Drawing, Point, circle, fillColor, filled, rectangle, text)
import Graphics.Drawing.Font (FontOptions, bold, font, italic, sansSerif)
import Math (pow, sin, cos, pi, (%))
import Random.LCG (mkSeed)
import Record.Extra (SLProxy(..), SNil)
import Test.QuickCheck.Gen (evalGen, shuffle)
import Type.Data.Graph (type (:/))
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

------
tempo = 72.0 :: Number

measure = 180.0 / tempo :: Number

crotchet = 60.0 / tempo :: Number

quaver = 30.0 / tempo :: Number

semiquaver = 15.0 / tempo :: Number

------
conv440 :: Number -> Number
conv440 i = 440.0 * (2.0 `pow` ((i - 69.0) / 12.0))

conv1 :: Number -> Number
conv1 i = 1.0 * (2.0 `pow` ((i - 0.0) / 12.0))

atT :: forall a. Number -> (Number -> a) -> (Number -> a)
atT t = lcmap (_ - t)

loopT :: forall a. Number -> (Number -> a) -> (Number -> a)
loopT t = lcmap (_ % t)

foldOverTime :: forall a b f. Foldable f => Applicative f => Monoid (f b) => (Number -> a -> b) -> (a -> Number) -> f a -> f b
foldOverTime trans getn = _.acc <<< foldl (\{ acc, clen } a -> { acc: acc <> (pure $ trans clen a), clen: clen + getn a }) { acc: mempty, clen: 0.0 }

boundPlayer :: forall a. Number -> (Number -> List a) -> Number -> List a
boundPlayer len a time = if time + kr >= 0.0 && time < (len) then a time else Nil

overZeroPlayer :: forall a. (Number -> List a) -> Number -> List a
overZeroPlayer = boundPlayer 100000.0 -- large enough...

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

----
kr = (toNumber defaultEngineInfo.msBetweenSamples) / 1000.0 :: Number

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

data ExplodeStage
  = ExplodeStage0 (Maybe Number)
  | ExplodeStage1 (Vec D4 (Maybe Number))

derive instance eqExplodeStage :: Eq ExplodeStage

data PlayerEvent
  = Triangle (Vec D3 (Maybe Number))
  | Square (Vec D4 (Maybe Number))
  | Motion (Either Point Point) -- resting point or offset from mouse
  | Rise (Vec D6 (Tuple Number (Maybe Number))) -- pos, stopped
  | Towards (Vec D4 (Either Point Number))
  | Explode ExplodeStage
  | Large (List (Tuple Point Number)) -- pos, startT
  | Bells (Vec D24 (Maybe Number))
  | Tether (Tuple Point Boolean) -- pos, holding
  | Gears (Vec D5 (Maybe Number))
  | Shrink (Vec D10 (Maybe Number))
  | Snow (Vec D42 (Maybe Number))
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
chooseVerseOne vc acc time = makeCanvas (acc { activity = HarmChooser { step: Row2Animation { startsAt: time, verseOne: vc } } }) time

chooseVerseTwo :: VerseChoice -> VerseChoice -> SilentNightAccumulator -> Number -> MakeCanvasT
chooseVerseTwo v1 vc acc time = makeCanvas (acc { activity = HarmChooser { step: Row3Animation { startsAt: time, verseOne: v1, verseTwo: vc } } }) time

chooseVerseThree :: VerseChoice -> VerseChoice -> VerseChoice -> SilentNightAccumulator -> Number -> MakeCanvasT
chooseVerseThree v1 v2 vc acc time = makeCanvas (acc { activity = HarmChooser { step: FadeOutAnimation { startsAt: time, verseOne: v1, verseTwo: v2, verseThree: vc } } }) time

makeCircleDim :: Number -> Number -> Number
makeCircleDim w i = ((w / circleDivisor) - (i * 3.0))

doVAction :: SilentNightAccumulator -> Number -> Number -> VerseChoice -> Boolean
doVAction acc w h vc =
  let
    i = (toNumber <<< versionToInt) vc

    dim = makeCircleDim w i

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

verseVersionChooser :: (VerseChoice -> SilentNightAccumulator -> Number -> MakeCanvasT) -> Number -> Number -> Drawing -> SilentNightAccumulator -> Number -> MakeCanvasT
verseVersionChooser vc w h d acc time
  | doVAction acc w h VersionOne = vc VersionOne acc time
  | doVAction acc w h VersionTwo = vc VersionTwo acc time
  | doVAction acc w h VersionThree = vc VersionThree acc time
  | doVAction acc w h VersionFour = vc VersionFour acc time
  | doVAction acc w h VersionFive = vc VersionFive acc time
  | doVAction acc w h VersionSix = vc VersionSix acc time
  | doVAction acc w h VersionSeven = vc VersionSeven acc time
  | doVAction acc w h VersionEight = vc VersionEight acc time
  | otherwise = pure $ Tuple acc (d <> circles w h (const 1.0) 1.0)

data HarmChooserStep
  = Row1Animation { startsAt :: Number }
  | Row1Choose
  | Row2Animation { startsAt :: Number, verseOne :: VerseChoice }
  | Row2Choose { verseOne :: VerseChoice }
  | Row3Animation { startsAt :: Number, verseOne :: VerseChoice, verseTwo :: VerseChoice }
  | Row3Choose { verseOne :: VerseChoice, verseTwo :: VerseChoice }
  | FadeOutAnimation { startsAt :: Number, verseOne :: VerseChoice, verseTwo :: VerseChoice, verseThree :: VerseChoice }

derive instance eqHarmChooserStep :: Eq HarmChooserStep

type SilentNightAccumulator
  = { initiatedClick :: Boolean
    , inClick :: Boolean
    , curClickId :: Maybe Int
    , mousePosition :: Maybe { x :: Number, y :: Number }
    , activity :: Activity
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

silentNightFadeIn = 0.0 :: Number

silentNightStay = silentNightFadeIn + fadeIn :: Number

silentNightFadeOut = silentNightStay + stay :: Number

silentNightDark = silentNightFadeOut + fadeOut :: Number

instructionFadeIn = silentNightDark + dark :: Number

instructionStay = instructionFadeIn + fadeIn :: Number

instructionFadeOut = instructionStay + stay :: Number

instructionDark = instructionFadeOut + fadeOut :: Number

circleArrives = 2.0 :: Number

circleFanOut = 3.0 :: Number

circleIntro = circleArrives + circleFanOut :: Number

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

cosRamp :: Number -> Number
cosRamp n = -0.5 * cos (n * pi) + 0.5

cosPRamp :: Number -> Number -> Number
cosPRamp p n = (-0.5 * cos (n * pi) + 0.5) `pow` p

circles :: Number -> Number -> (VerseChoice -> Number) -> Number -> Drawing
circles w h opq traj = fold (map (\i' -> let i = (toNumber <<< versionToInt) i' in filled (fillColor (whiteRGBA (opq i'))) (circle ((2.0 * i * traj + 1.0) * w / 16.0) h (makeCircleDim w i))) verseChoices)

circleFanner :: Number -> Number -> Number -> Number -> Drawing
circleFanner w h startsAt time = let nTime = time - startsAt in circles w h (\v -> if nTime < circleArrives then (if v == VersionOne then (nTime / circleArrives) else 0.0) else 1.0) (if nTime < circleArrives then 0.0 else cosPRamp 1.4 $ min 1.0 ((nTime - circleArrives) / circleFanOut))

circleChoice :: Number -> Number -> VerseChoice -> Number -> Number -> Drawing
circleChoice w h vc startsAt time = circles w h (\v -> if v == vc then 1.0 else max 0.3 (1.0 - (time - startsAt) / (0.7 * circleFade))) 1.0

firstRow :: Number -> Number
firstRow h = h / 6.0

secondRow :: Number -> Number
secondRow h = 3.0 * h / 6.0

thirdRow :: Number -> Number
thirdRow h = 5.0 * h / 6.0

circleChosen :: Number -> Number -> VerseChoice -> Drawing
circleChosen w h vc = circles w h (\v -> if v == vc then 1.0 else 0.3) 1.0

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

    x = calcSlope 0.0 x0 1.0 x1 (cosPRamp 1.7 $ (time - startsAt) / circleFlyAway)

    y = calcSlope 0.0 y0 1.0 y1 (cosPRamp 1.7 $ (time - startsAt) / circleFlyAway)
  in
    filled (fillColor (whiteRGBA opq)) (circle x y (makeCircleDim w i))

type MakeCanvasT
  = Reader { evts :: Array PlayerEvent, w :: Number, h :: Number } (Tuple SilentNightAccumulator Drawing)

standardIntro = 1.0 :: Number

standardOutro = 3.0 :: Number

standardPress = 0.5 :: Number

motionNormal = 5.0 :: Number

riseNormal = 4.0 :: Number

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
sinp v p = (v * ((sin (p)) * 0.5 + 0.5) * 0.9 + 0.05)

cosp :: Number -> Number -> Number
cosp v p = (v * ((cos (p)) * 0.5 + 0.5) * 0.9 + 0.05)

sqToRect :: Number -> Number -> Number -> Rectangle
sqToRect x y r = { x: x - r, y: y - r, width: 2.0 * r, height: 2.0 * r }

nextObj :: forall n a. Nat n => (Vec n a -> PlayerEvent) -> SilentNightAccumulator -> SilentNightPlayerT -> (Number -> Vec n a) -> Number -> MakeCanvasT
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

nextMotion :: SilentNightAccumulator -> SilentNightPlayerT -> Either Point Point -> Number -> MakeCanvasT
nextMotion acc i epp time =
  makeCanvas
    ( acc
        { activity =
          SilentNightPlayer
            ( i
                { playerEvents = [ Motion epp ] <> drop 1 i.playerEvents
                }
            )
        }
    )
    time

mouseOrBust :: Maybe Point -> Point
mouseOrBust = fromMaybe { x: 0.0, y: 0.0 }

riseXP = [ 1.0 / 12.0, 3.0 / 12.0, 5.0 / 12.0, 7.0 / 12.0, 9.0 / 12.0, 11.0 / 12.0 ] :: Array Number

makeCanvas :: SilentNightAccumulator -> Number -> MakeCanvasT
makeCanvas acc time = do
  { w, h } <- ask
  map (over _2 (append (filled (fillColor (rgb 0 0 0)) (rectangle 0.0 0.0 w h)))) (go w h)
  where
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
                  (w / 2.0 - if time > silentNightDark then 100.0 else 120.0)
                  (h / 2.0)
                  (fillColor (whiteRGBA (introOpacity time)))
                  if time > silentNightDark then "Click on or press the circles" else "Silent Night"
              )
    HarmChooser { step } -> case step of
      Row1Animation i ->
        if time > i.startsAt + circleIntro then
          makeCanvas (acc { activity = HarmChooser { step: Row1Choose } }) time
        else
          pure $ Tuple acc (circleFanner w (firstRow h) i.startsAt time)
      Row1Choose -> verseVersionChooser chooseVerseOne w (firstRow h) mempty acc time
      Row2Animation i ->
        if time > i.startsAt + circleIntro then
          makeCanvas (acc { activity = HarmChooser { step: Row2Choose { verseOne: i.verseOne } } }) time
        else
          pure $ Tuple acc (circleFanner w (secondRow h) i.startsAt time <> circleChoice w (firstRow h) i.verseOne i.startsAt time)
      Row2Choose i -> verseVersionChooser (chooseVerseTwo i.verseOne) w (secondRow h) (circleChosen w (firstRow h) i.verseOne) acc time
      Row3Animation i ->
        if time > i.startsAt + circleIntro then
          makeCanvas (acc { activity = HarmChooser { step: Row3Choose { verseOne: i.verseOne, verseTwo: i.verseTwo } } }) time
        else
          pure $ Tuple acc (circleFanner w (thirdRow h) i.startsAt time <> circleChoice w (secondRow h) i.verseTwo i.startsAt time <> circleChosen w (firstRow h) i.verseOne)
      Row3Choose i -> verseVersionChooser (chooseVerseThree i.verseOne i.verseTwo) w (thirdRow h) (circleChosen w (firstRow h) i.verseOne <> circleChosen w (secondRow h) i.verseTwo) acc time
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
        Triangle v ->
          let
            sqv = sequence v

            top = V.index v d0

            left = V.index v d1

            right = V.index v d2

            cw = w / 14.0

            twoCw = 2.0 * cw

            nextTriangle = nextObj Triangle

            o
              | maybe false (\s -> time > standardOutro + foldl max 0.0 s) sqv = newCanvas i acc time
              | time < i.eventStart + standardIntro =
                pure
                  $ ( Tuple acc
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
                                        ( circle (sinp w pdpi)
                                            (cosp h pdpi)
                                            cw
                                        )
                                )
                                [ 0.0, 2.0 / 3.0, 4.0 / 3.0 ]
                            )
                    )
              | doAction
                  acc
                  (sqToRect (sinp w (0.0 * pi)) (cosp h (0.0 * pi)) cw)
                  && top
                  == Nothing = nextTriangle acc i (\t -> ((Just t) +> left +> right +> empty)) time
              | doAction
                  acc
                  (sqToRect (sinp w (2.0 * pi / 3.0)) (cosp h (2.0 * pi / 3.0)) cw)
                  && left
                  == Nothing = nextTriangle acc i (\t -> (top +> (Just t) +> right +> empty)) time
              | doAction
                  acc
                  (sqToRect (sinp w (4.0 * pi / 3.0)) (cosp h (4.0 * pi / 3.0)) cw)
                  && right
                  == Nothing = nextTriangle acc i (\t -> (top +> left +> (Just t) +> empty)) time
              | otherwise =
                pure
                  $ ( Tuple acc
                        $ fold
                            ( map
                                ( \(Tuple pd n) ->
                                    let
                                      pdpi = (pd + maybe 0.0 (\s -> (time - foldl max 0.0 s) `pow` 1.6) sqv) * pi

                                      r = case n of
                                        Nothing -> cw
                                        Just n' -> pressEffect cw standardPress (time - n')
                                    in
                                      filled
                                        ( fillColor
                                            ( whiteRGBA
                                                ( case n of
                                                    Nothing -> 1.0
                                                    Just n' -> min 0.4 (1.0 - (0.6 * (time - n') / standardPress))
                                                )
                                            )
                                        )
                                        ( circle
                                            (sinp w pdpi)
                                            (cosp h pdpi)
                                            r
                                        )
                                )
                                [ Tuple 0.0 top
                                , Tuple (2.0 / 3.0) left
                                , Tuple (4.0 / 3.0) right
                                ]
                            )
                    )
          in
            o
        Square v ->
          let
            sqv = sequence v

            topLeft = V.index v d0

            topRight = V.index v d1

            bottomLeft = V.index v d2

            bottomRight = V.index v d3

            cw = w / 14.0

            twoCw = 2.0 * cw

            nextSquare = nextObj Square

            o
              | maybe false (\s -> time > standardOutro + foldl max 0.0 s) sqv = newCanvas i acc time
              | time < i.eventStart + standardIntro =
                pure
                  $ ( Tuple acc
                        $ fold
                            ( map
                                ( \{ x, y } ->
                                    filled (fillColor (whiteRGBA ((time - i.eventStart) / standardIntro)))
                                      (circle (x * w) (y * h) cw)
                                )
                                [ { x: 0.2, y: 0.2 }, { x: 0.8, y: 0.2 }, { x: 0.2, y: 0.8 }, { x: 0.8, y: 0.8 } ]
                            )
                    )
              | doAction
                  acc
                  (sqToRect 0.2 0.2 cw)
                  && topLeft
                  == Nothing = nextSquare acc i (\t -> ((Just t) +> topRight +> bottomLeft +> bottomRight +> empty)) time
              | doAction
                  acc
                  (sqToRect 0.8 0.2 cw)
                  && topRight
                  == Nothing = nextSquare acc i (\t -> (topLeft +> (Just t) +> bottomLeft +> bottomRight +> empty)) time
              | doAction
                  acc
                  (sqToRect 0.2 0.8 cw)
                  && bottomLeft
                  == Nothing = nextSquare acc i (\t -> (topLeft +> topRight +> (Just t) +> bottomRight +> empty)) time
              | doAction
                  acc
                  (sqToRect 0.8 0.8 cw)
                  && bottomRight
                  == Nothing = nextSquare acc i (\t -> (topLeft +> topRight +> bottomLeft +> (Just t) +> empty)) time
              | otherwise =
                pure
                  $ ( Tuple acc
                        $ fold
                            ( map
                                ( \(Tuple { x0, y0, x1, y1 } n) ->
                                    let
                                      nowT = maybe 0.0 (\s -> (time - foldl max 0.0 s)) sqv

                                      r = case n of
                                        Nothing -> cw
                                        Just n' -> pressEffect cw standardPress (time - n')
                                    in
                                      filled
                                        ( fillColor
                                            ( whiteRGBA
                                                ( case n of
                                                    Nothing -> 1.0
                                                    Just n' -> min 0.4 (1.0 - (0.6 * (time - n') / standardPress))
                                                )
                                            )
                                        )
                                        ( circle
                                            (w * calcSlope 0.0 x0 standardOutro x1 nowT)
                                            (h * calcSlope 0.0 y0 standardOutro y1 nowT)
                                            r
                                        )
                                )
                                [ Tuple { x0: 0.2, y0: 0.2, x1: 1.1, y1: 1.1 } topLeft
                                , Tuple { x0: 0.8, y0: 0.2, x1: -0.1, y1: -0.1 } topRight
                                , Tuple { x0: 0.2, y0: 0.8, x1: 1.1, y1: 1.1 } bottomLeft
                                , Tuple { x0: 0.8, y0: 0.8, x1: -0.1, y1: -0.1 } bottomRight
                                ]
                            )
                    )
          in
            o
        Motion lr ->
          let
            cw = w / 6.0

            il = isLeft lr

            ir = not il

            xp = (either (\v -> w * v.x) (\v -> (mouseOrBust acc.mousePosition).x - v.x) lr)

            yp = (either (\v -> h * v.y) (\v -> (mouseOrBust acc.mousePosition).y - v.y) lr)

            o
              | time > i.eventStart + standardIntro + motionNormal + standardOutro = newCanvas i acc time
              | time < i.eventStart + standardIntro =
                pure
                  $ Tuple acc
                      ( filled
                          (fillColor (whiteRGBA (min 1.0 $ (time - i.eventStart) / standardIntro)))
                          (circle xp yp cw)
                      )
              | either
                  ( \v ->
                      doingAction
                        acc
                        (sqToRect xp yp cw)
                  )
                  (const false)
                  lr = nextMotion acc i (Right { x: (mouseOrBust acc.mousePosition).x - xp, y: (mouseOrBust acc.mousePosition).y - yp }) time
              | either
                  (const false)
                  (const $ not acc.inClick)
                  lr = nextMotion acc i (Left { x: xp, y: yp }) time
              | otherwise =
                pure
                  $ Tuple acc
                      ( filled
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
          in
            o
        Rise v ->
          let
            one@(Tuple oneN oneS) = V.index v d0

            two@(Tuple twoN twoS) = V.index v d1

            three@(Tuple threeN threeS) = V.index v d2

            four@(Tuple fourN fourS) = V.index v d3

            five@(Tuple fiveN fiveS) = V.index v d4

            six@(Tuple sixN sixS) = V.index v d5

            cw = w / 16.0

            nextRise = nextObj Rise

            twoCw = 2.0 * cw

            tillNormal = i.eventStart + standardIntro + riseNormal

            o
              | time > tillNormal + standardOutro = newCanvas i acc time
              | time < i.eventStart + standardIntro =
                pure
                  $ Tuple acc
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
              | isNothing oneS
                  && doAction
                      acc
                      (sqToRect (1.0 * w / 12.0) (oneN * h) cw) = nextRise acc i (\t -> Tuple oneN (Just time) +> two +> three +> four +> five +> six +> empty) time
              | isNothing twoS
                  && doAction
                      acc
                      (sqToRect (3.0 * w / 12.0) (twoN * h) cw) = nextRise acc i (\t -> one +> Tuple twoN (Just time) +> three +> four +> five +> six +> empty) time
              | isNothing threeS
                  && doAction
                      acc
                      (sqToRect (5.0 * w / 12.0) (threeN * h) cw) = nextRise acc i (\t -> one +> two +> Tuple threeN (Just time) +> four +> five +> six +> empty) time
              | isNothing fourS
                  && doAction
                      acc
                      (sqToRect (7.0 * w / 12.0) (fourN * h) cw) = nextRise acc i (\t -> one +> two +> three +> Tuple fourN (Just time) +> five +> six +> empty) time
              | isNothing fiveS
                  && doAction
                      acc
                      (sqToRect (9.0 * w / 12.0) (fiveN * h) cw) = nextRise acc i (\t -> one +> two +> three +> four +> Tuple fiveN (Just time) +> six +> empty) time
              | isNothing sixS
                  && doAction
                      acc
                      (sqToRect (11.0 * w / 12.0) (sixN * h) cw) = nextRise acc i (\t -> one +> two +> three +> four +> five +> Tuple sixN (Just time) +> empty) time
              | otherwise =
                pure
                  $ Tuple acc
                      ( fold
                          ( map
                              ( \(Tuple xp stopped) ->
                                  ( filled
                                      (fillColor (whiteRGBA 1.0))
                                      (circle (w * xp) (h * (calcSlope (i.eventStart + standardIntro) 0.9 (tillNormal) 0.1 (fromMaybe (min time tillNormal) stopped))) cw)
                                  )
                              )
                              (zip riseXP [ oneS, twoS, threeS, fourS, fiveS, sixS ])
                          )
                      )
          in
            o
        _ -> pure $ Tuple acc mempty

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
    acc =
      acc'
        { mousePosition =
          ( \{ x, y } ->
              { x: x - ci.boundingClientRect.x, y: y - ci.boundingClientRect.y
              }
          )
            <$> head p.interactions
        , initiatedClick = (_.id <$> head p.interactions) /= acc'.curClickId
        , inClick = p.nInteractions /= 0
        , curClickId = _.id <$> head p.interactions
        }

    (Tuple vizAcc cvs) = runReader (makeCanvas acc time) { evts, w: ci.w, h: ci.h }

    players = Nil

riseStart = Tuple 0.0 Nothing :: Tuple Number (Maybe Number)

allPlayerEvent =
  [ Triangle (fill (const Nothing))
  , Square (fill (const Nothing))
  , Motion (Left { x: 0.15, y: 0.15 })
  , Rise (fill (const riseStart))
  , Towards
      ( Left { x: 0.15, y: 0.15 }
          +> Left { x: 0.85, y: 0.15 }
          +> Left { x: 0.15, y: 0.85 }
          +> Left { x: 0.85, y: 0.85 }
          +> empty
      )
  , Explode (ExplodeStage0 Nothing)
  , Large Nil
  , Bells (fill (const Nothing))
  , Tether (Tuple { x: 0.5, y: 0.5 } false)
  , Gears (fill (const Nothing))
  , Shrink (fill (const Nothing))
  , Snow (fill (const Nothing))
  ] ::
    Array PlayerEvent

main :: Klank' SilentNightAccumulator
main =
  klank
    { run =
      runInBrowser_ do
        inter <- getInteractivity
        (Milliseconds timeNow) <- map unInstant now
        let
          evts' = evalGen (shuffle allPlayerEvent) { newSeed: mkSeed (floor timeNow), size: 10 }
        evts <-
          sequence
            $ map
                ( \i -> do
                    n <- random
                    pure [ NoEvent (n * 3.5 + 1.0), i ]
                )
                evts'
        pure $ scene inter (join evts)
    , accumulator =
      \res _ ->
        res
          { initiatedClick: false
          , curClickId: Nothing
          , mousePosition: Nothing
          , activity: HarmChooser { step: Row1Animation { startsAt: 0.0 } } -- Intro
          , inClick: false
          }
    , exporter = defaultExporter
    , buffers =
      makeBuffersKeepingCache
        []
    }

newtype Interactions
  = Interactions
  { interactions :: Ref.Ref (InteractionOnsets)
  , nInteractions :: Ref.Ref Int
  , dispose :: Effect Unit
  }

type InteractionOnsets
  = Array
      { id :: Int
      , x :: Number
      , y :: Number
      }

handleTE :: Int -> Ref.Ref (InteractionOnsets) -> TouchEvent -> Effect Unit
handleTE i ref te = do
  let
    ts = changedTouches te
  let
    l = TL.length ts
  let
    tlist = map (\t -> { id: i, x: toNumber $ T.clientX t, y: toNumber $ T.clientY t }) (catMaybes $ map (\x -> TL.item x ts) (range 0 (l - 1)))
  void $ Ref.modify (\ipt -> tlist <> ipt) ref

handleME :: Int -> Ref.Ref (InteractionOnsets) -> MouseEvent -> Effect Unit
handleME id ref me = do
  let
    x = toNumber $ ME.clientX me
  let
    y = toNumber $ ME.clientY me
  void $ Ref.modify (\ipt -> [ { id, x, y } ] <> ipt) ref

getInteractivity :: Effect Interactions
getInteractivity = do
  w <- window
  nav <- navigator w
  ua <- userAgent nav
  let
    mobile = isJust (indexOf (Pattern "iPhone") ua) || isJust (indexOf (Pattern "iPad") ua) || isJust (indexOf (Pattern "Android") ua)
  nInteractions <- Ref.new 0
  totalInteractions <- Ref.new 0
  interactions <- Ref.new []
  target <- toEventTarget <$> window
  touchStartListener <-
    eventListener \e -> do
      fromEvent e
        # traverse_ \me -> do
            void $ Ref.modify (_ + 1) nInteractions
            nt <- Ref.modify (_ + 1) totalInteractions
            handleTE nt interactions me
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
            handleME nt interactions me
  mouseUpListener <-
    eventListener \e -> do
      ME.fromEvent e
        # traverse_ \me -> do
            void $ Ref.modify (_ - 1) nInteractions
  if mobile then do
    addEventListener (wrap "touchstart") touchStartListener false target
    addEventListener (wrap "touchend") touchEndListener false target
  else do
    addEventListener (wrap "mousedown") mouseDownListener false target
    addEventListener (wrap "mouseup") mouseUpListener false target
  let
    dispose =
      if mobile then do
        removeEventListener (wrap "touchstart") touchStartListener false target
        removeEventListener (wrap "touchend") touchEndListener false target
      else do
        removeEventListener (wrap "mousedown") mouseDownListener false target
        removeEventListener (wrap "mouseup") mouseUpListener false target
  pure (Interactions { interactions, nInteractions, dispose })

withInteractions ::
  forall a.
  Interactions ->
  Event a ->
  Event { value :: a, interactions :: InteractionOnsets, nInteractions :: Int }
withInteractions (Interactions { interactions, nInteractions }) e =
  makeEvent \k ->
    e
      `subscribe`
        \value -> do
          interactionsValue <- Ref.read interactions
          nInteractionsValue <- Ref.read nInteractions
          k { value, interactions: interactionsValue, nInteractions: nInteractionsValue }

interactionLog :: Interactions -> Behavior ({ interactions :: InteractionOnsets, nInteractions :: Int })
interactionLog m = behavior \e -> map (\{ value, interactions, nInteractions } -> value { interactions, nInteractions }) (withInteractions m e)
