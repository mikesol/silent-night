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
  | Motion (Maybe Point) -- resting point or offset from mouse
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
makeCircleDim w i = calcSlope 0.0 (w / circleDivisor) 7.0 (w / (circleDivisor * 2.0)) i

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

circles :: Number -> Number -> (VerseChoice -> Number) -> Number -> Drawing
circles w h opq traj = fold (map (\i' -> let i = (toNumber <<< versionToInt) i' in filled (fillColor (whiteRGBA (opq i'))) (circle ((2.0 * i * traj + 1.0) * w / 16.0) h (makeCircleDim w i))) verseChoices)

circles' :: Number -> Number -> (Number -> Number) -> Drawing
circles' w h traj = fold (map (\i' -> let i = (toNumber <<< versionToInt) i' in filled (fillColor (whiteRGBA 1.0)) (circle ((2.0 * i + 1.0) * w / 16.0) h (makeCircleDim w i * (traj i)))) verseChoices)

circleFanner :: Number -> Number -> Number -> Number -> Drawing
circleFanner w h startsAt time =
  let
    nTime = time - startsAt
  in
    circles' w h
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

    x = calcSlope 0.0 x0 1.0 x1 (pnCurve 0.8 $ (time - startsAt) / circleFlyAway)

    y = calcSlope 0.0 y0 1.0 y1 (pnCurve 0.8 $ (time - startsAt) / circleFlyAway)
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
sinp v p = (v * (((sin (p)) * 0.5 + 0.5) * 0.7 + 0.15))

cosp :: Number -> Number -> Number
cosp v p = (v * (((cos (p)) * 0.5 + 0.5) * 0.7 + 0.15))

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

nextMotion :: SilentNightAccumulator -> SilentNightPlayerT -> Maybe Point -> Number -> MakeCanvasT
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
  map (over _2 (append (filled (fillColor (rgb 0 0 0)) (rectangle 0.0 0.0 w h) <> (fold (map (\f -> f w h time) starFs))))) (go w h)
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

            cw = w / 16.0

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
                  && doAction
                      acc
                      (sqToRect (sinp w (0.0 * pi)) (cosp h (0.0 * pi)) cw) = nextTriangle acc i (\t -> ((Just t) +> left +> right +> empty)) time
              | left
                  == Nothing
                  && doAction
                      acc
                      (sqToRect (sinp w (2.0 * pi / 3.0)) (cosp h (2.0 * pi / 3.0)) cw) = nextTriangle acc i (\t -> (top +> (Just t) +> right +> empty)) time
              | right
                  == Nothing
                  && doAction
                      acc
                      (sqToRect (sinp w (4.0 * pi / 3.0)) (cosp h (4.0 * pi / 3.0)) cw) = nextTriangle acc i (\t -> (top +> left +> (Just t) +> empty)) time
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

            c1 = 0.2

            c2 = 0.8

            topLeft = V.index v d0

            topRight = V.index v d1

            bottomLeft = V.index v d2

            bottomRight = V.index v d3

            cw = w / 17.0

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
                                [ { x: c1, y: c1 }, { x: c2, y: c1 }, { x: c1, y: c2 }, { x: c2, y: c2 } ]
                            )
                    )
              | topLeft
                  == Nothing
                  && doAction
                      acc
                      (sqToRect (c1 * w) (c1 * h) cw) = nextSquare acc i (\t -> ((Just t) +> topRight +> bottomLeft +> bottomRight +> empty)) time
              | topRight
                  == Nothing
                  && doAction
                      acc
                      (sqToRect (c2 * w) (c1 * h) cw) = nextSquare acc i (\t -> (topLeft +> (Just t) +> bottomLeft +> bottomRight +> empty)) time
              | bottomLeft
                  == Nothing
                  && doAction
                      acc
                      (sqToRect (c1 * w) (c2 * h) cw) = nextSquare acc i (\t -> (topLeft +> topRight +> (Just t) +> bottomRight +> empty)) time
              | bottomRight
                  == Nothing
                  && doAction
                      acc
                      (sqToRect (c2 * w) (c2 * h) cw) = nextSquare acc i (\t -> (topLeft +> topRight +> bottomLeft +> (Just t) +> empty)) time
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
                                                    Just n' -> maybe' (\_ -> max 0.4 (1.0 - (0.6 * (time - n') / standardPress))) (\s -> let mx = foldl max 0.0 s in calcSlope mx (if n' == mx then 1.0 else 0.4) (mx + standardOutro) 0.0 time) sqv
                                                )
                                            )
                                        )
                                        ( circle
                                            (w * calcSlope 0.0 x0 standardOutro x1 nowT)
                                            (h * calcSlope 0.0 y0 standardOutro y1 nowT)
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
        Motion m ->
          let
            cw = w / 16.0

            xp = maybe' (\_ -> (mouseOrBust acc.mousePosition).x) (\v -> w * v.x) m

            yp = maybe' (\_ -> (mouseOrBust acc.mousePosition).x) (\v -> h * v.y) m

            o
              | time > i.eventStart + standardIntro + motionNormal + standardOutro = newCanvas i acc time
              | time < i.eventStart + standardIntro =
                pure
                  $ Tuple acc
                      ( filled
                          (fillColor (whiteRGBA (min 1.0 $ (time - i.eventStart) / standardIntro)))
                          (circle xp yp cw)
                      )
              | maybe
                  false
                  ( const
                      $ doingAction
                          acc
                          (sqToRect xp yp cw)
                  )
                  m = nextMotion acc i Nothing time
              | maybe
                  (not acc.inClick)
                  (const $ false)
                  m = nextMotion acc i (Just { x: xp, y: yp }) time
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
  , Motion (Just { x: 0.18, y: 0.18 })
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

acC = acx [ Triangle (Nothing +> Nothing +> Nothing +> empty) ] :: Activity

acD = acx [ Square (Nothing +> Nothing +> Nothing +> Nothing +> empty) ] :: Activity

acE = acx [ Motion (Just { x: 0.18, y: 0.18 }) ] :: Activity

acF = acx [ Rise (fill $ const riseStart) ] :: Activity

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
          , activity: acE
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

-- print(",".join(["{x:"+str(random.random())+",y:"+str(random.random())+",f: \\x -> "+str(random.random()*0.9+0.1)+" * sin ("+str(random.random()*0.6 + 0.1)+" * (x + "+str(random.random())+") * pi) }" for x in range(100)]))
starDs :: Array { x :: Number, y :: Number, f :: Number -> Number }
starDs = [ { x: 0.7998979153795341, y: 0.08385139594872981, f: \x -> 0.7657069009122556 * sin (0.12048910397521838 * (x + 0.9235025221582407) * pi) }, { x: 0.1889061340397945, y: 0.41662576058498235, f: \x -> 0.1415196387959073 * sin (0.5683357566654806 * (x + 0.4140936479451277) * pi) }, { x: 0.6415434619211883, y: 0.5708146975512279, f: \x -> 0.11577248196435784 * sin (0.29554293868038256 * (x + 0.3836381290510813) * pi) }, { x: 0.46408624003822874, y: 0.5352411436249525, f: \x -> 0.27899259750022887 * sin (0.6168518732109306 * (x + 0.377444452261138) * pi) }, { x: 0.9372148362501318, y: 0.3739296461256748, f: \x -> 0.671489098410653 * sin (0.4822310763852671 * (x + 0.3908243855120592) * pi) }, { x: 0.9677579259565381, y: 0.07151980503063837, f: \x -> 0.9752455547563101 * sin (0.6051350059573541 * (x + 0.46951319522277657) * pi) }, { x: 0.5491926546092344, y: 0.07374039190919535, f: \x -> 0.33536560976485197 * sin (0.53634515525111 * (x + 0.3411527658666662) * pi) }, { x: 0.522532549928989, y: 0.24000747216531726, f: \x -> 0.6849413502333674 * sin (0.25061020850894333 * (x + 0.2704271222503388) * pi) }, { x: 0.676735264877141, y: 0.154014527584188, f: \x -> 0.643492816957883 * sin (0.4999621556849063 * (x + 0.23972440926252403) * pi) }, { x: 0.022005599332260095, y: 0.4517846398419565, f: \x -> 0.9780033023620383 * sin (0.5690010434724488 * (x + 0.4700260516669268) * pi) }, { x: 0.6623321309130382, y: 0.1538961699400414, f: \x -> 0.39547598751018875 * sin (0.35752515144399377 * (x + 0.8345066395190842) * pi) }, { x: 0.07008140643788907, y: 0.5088425430429082, f: \x -> 0.938078599349549 * sin (0.21111028367327936 * (x + 0.8390425040791938) * pi) }, { x: 0.8183208236713514, y: 0.12015800392666187, f: \x -> 0.8959988138316818 * sin (0.6783042097542152 * (x + 0.003916202043623196) * pi) }, { x: 0.6567155163250299, y: 0.8911844616174074, f: \x -> 0.23654036104506207 * sin (0.2682238556417563 * (x + 0.4334644233731685) * pi) }, { x: 0.6635142395130955, y: 0.16220416064115362, f: \x -> 0.8813181743932739 * sin (0.545982341474434 * (x + 0.8337268747684451) * pi) }, { x: 0.27523028020466733, y: 0.01542831693086355, f: \x -> 0.8125688271194221 * sin (0.11256935190258262 * (x + 0.6476702955558028) * pi) }, { x: 0.5999468031487667, y: 0.9508123847205505, f: \x -> 0.12455506698851554 * sin (0.16642138506387108 * (x + 0.9842802977680329) * pi) }, { x: 0.5264600473652116, y: 0.2521314595090074, f: \x -> 0.6004655754729078 * sin (0.5488467917947338 * (x + 0.4100251478253423) * pi) }, { x: 0.2368072536753597, y: 0.3972845965548708, f: \x -> 0.7391259416146705 * sin (0.43050261122448596 * (x + 0.3697052483399993) * pi) }, { x: 0.17679605291326028, y: 0.6268540032189557, f: \x -> 0.6546348882779175 * sin (0.6721869642094553 * (x + 0.24128601888888124) * pi) }, { x: 0.8743480466826442, y: 0.0840467624458574, f: \x -> 0.48785103770632665 * sin (0.3933065653420563 * (x + 0.7481526400188201) * pi) }, { x: 0.061704426811517776, y: 0.34095357496002165, f: \x -> 0.7952378622812539 * sin (0.2073614887237352 * (x + 0.07236472200581767) * pi) }, { x: 0.046728095535261915, y: 0.3557906691124455, f: \x -> 0.8471468676835398 * sin (0.6936312563009017 * (x + 0.8884226981678148) * pi) }, { x: 0.9420065262296728, y: 0.46984187619709383, f: \x -> 0.9148056281198275 * sin (0.21228333966864504 * (x + 0.12171006947395191) * pi) }, { x: 0.22828963814693148, y: 0.5936010890996245, f: \x -> 0.5869924335309348 * sin (0.6760951901932156 * (x + 0.42581741442330057) * pi) }, { x: 0.9448422412741705, y: 0.847273328594833, f: \x -> 0.1444443072829259 * sin (0.564055969475472 * (x + 0.23828946968648346) * pi) }, { x: 0.026017619239415812, y: 0.38784732596558724, f: \x -> 0.40568046115459055 * sin (0.5429357146134501 * (x + 0.17243981005583087) * pi) }, { x: 0.8600314612590836, y: 0.16872127332155784, f: \x -> 0.5243336361413855 * sin (0.5770135444505058 * (x + 0.5096273590873813) * pi) }, { x: 0.6380405298096601, y: 0.47804272716920826, f: \x -> 0.9577493093614781 * sin (0.2636125952601193 * (x + 0.22133762993410544) * pi) }, { x: 0.9623176544984414, y: 0.5731719748334171, f: \x -> 0.8510815994212508 * sin (0.23713905867729537 * (x + 0.9659410292888134) * pi) }, { x: 0.2715621864828037, y: 0.5746086436300603, f: \x -> 0.2476824596259333 * sin (0.6883522844630905 * (x + 0.8934386328562979) * pi) }, { x: 0.9163545335851067, y: 0.5876324876520401, f: \x -> 0.29767964201568 * sin (0.32407987185183196 * (x + 0.5624478725260176) * pi) }, { x: 0.5121105811687144, y: 0.5976398842996643, f: \x -> 0.9487706274379716 * sin (0.3782745410559547 * (x + 0.005825894962646516) * pi) }, { x: 0.42866822912910885, y: 0.6830883083393303, f: \x -> 0.22875670760621292 * sin (0.1725422234511803 * (x + 0.4988007813609098) * pi) }, { x: 0.581878431057003, y: 0.9246881434525345, f: \x -> 0.5571826049082016 * sin (0.664291356515955 * (x + 0.4928719048473047) * pi) }, { x: 0.3356527164199967, y: 0.29328180629071976, f: \x -> 0.845038335641492 * sin (0.695613646890855 * (x + 0.9700923098200159) * pi) }, { x: 0.10749775381328286, y: 0.5554202833939391, f: \x -> 0.739285784528385 * sin (0.6469822932763877 * (x + 0.17466536383933384) * pi) }, { x: 0.8950320550823506, y: 0.6678599725651887, f: \x -> 0.503171785472261 * sin (0.28611277693258097 * (x + 0.6473420586463666) * pi) }, { x: 0.1443247940739063, y: 0.010924436297578888, f: \x -> 0.6986998672144634 * sin (0.17255867274134484 * (x + 0.12993132146898623) * pi) }, { x: 0.2791503707404528, y: 0.06312099128646453, f: \x -> 0.36429082463506834 * sin (0.34196259679059626 * (x + 0.6979922700561009) * pi) }, { x: 0.9285299556309622, y: 0.10161382562297627, f: \x -> 0.8084105943491684 * sin (0.3347280804116324 * (x + 0.649834380138763) * pi) }, { x: 0.5087841904358481, y: 0.5148341219339472, f: \x -> 0.12821142080843256 * sin (0.6436397049542052 * (x + 0.49377239663519) * pi) }, { x: 0.5767279353705533, y: 0.5017259429620036, f: \x -> 0.7074152315694675 * sin (0.24359373059476075 * (x + 0.3640710968785963) * pi) }, { x: 0.3572829307534393, y: 0.07930845199331438, f: \x -> 0.12272185939340002 * sin (0.21496787160199268 * (x + 0.8232597611434008) * pi) }, { x: 0.3045478568273394, y: 0.15189984836967618, f: \x -> 0.44456300688412687 * sin (0.5905856604494417 * (x + 0.6019971776570228) * pi) }, { x: 0.43551591756677743, y: 0.7888726012495775, f: \x -> 0.39358983003016523 * sin (0.46177449026369366 * (x + 0.6330979653144985) * pi) }, { x: 0.8996122943728967, y: 0.4193294174077634, f: \x -> 0.20922101207535942 * sin (0.2505708338272897 * (x + 0.6669132311420939) * pi) }, { x: 0.006809687407536957, y: 0.2591751351787317, f: \x -> 0.4990002170158462 * sin (0.16376093348549342 * (x + 0.9917153566334772) * pi) }, { x: 0.28360533801009014, y: 0.8965273384199693, f: \x -> 0.832548216777215 * sin (0.6968597343485691 * (x + 0.6522250227849495) * pi) }, { x: 0.11768473063078866, y: 0.47431612674759016, f: \x -> 0.3069972229715307 * sin (0.35402930954677736 * (x + 0.2598129062355331) * pi) }, { x: 0.9605409025944893, y: 0.9210189781644056, f: \x -> 0.13767653252020387 * sin (0.645397983120965 * (x + 0.20332445324936355) * pi) }, { x: 0.5802648295815812, y: 0.32188272064836765, f: \x -> 0.7926623083133738 * sin (0.5024237429565865 * (x + 0.43358317968813154) * pi) }, { x: 0.460669273896289, y: 0.826858493037314, f: \x -> 0.6875062564366267 * sin (0.32204424860440284 * (x + 0.6846140018360137) * pi) }, { x: 0.49649673809381545, y: 0.5368631031674957, f: \x -> 0.45981852239312815 * sin (0.4007905113743796 * (x + 0.3419384588612011) * pi) }, { x: 0.014953390603937744, y: 0.7378416524667052, f: \x -> 0.6140466078126797 * sin (0.4870594133670276 * (x + 0.5499076425091735) * pi) }, { x: 0.9651492489337455, y: 0.034190311101410464, f: \x -> 0.680725250861211 * sin (0.1404764045758468 * (x + 0.027344375850516744) * pi) }, { x: 0.3489646579749772, y: 0.45094294411398583, f: \x -> 0.4762308773272914 * sin (0.47885316186142957 * (x + 0.4348305860227125) * pi) }, { x: 0.721516436343126, y: 0.30602540798118605, f: \x -> 0.9539093982726269 * sin (0.12257732836191289 * (x + 0.3236116142830592) * pi) }, { x: 0.0361505728383702, y: 0.22671487550392044, f: \x -> 0.5470863801997212 * sin (0.2267991542971995 * (x + 0.9817698978873135) * pi) }, { x: 0.383638361887656, y: 0.3918972451520907, f: \x -> 0.34106319921048894 * sin (0.6698729229823568 * (x + 0.9500835732982084) * pi) }, { x: 0.19432877382630098, y: 0.9522640114916134, f: \x -> 0.5234030615472549 * sin (0.29322018355964974 * (x + 0.3719893915176793) * pi) }, { x: 0.46657705708863173, y: 0.12174792367473619, f: \x -> 0.7047731530960449 * sin (0.39527154230290673 * (x + 0.5936262233984391) * pi) }, { x: 0.21703176126522317, y: 0.8930891192173489, f: \x -> 0.33641264539117166 * sin (0.4493531696446884 * (x + 0.2517156629417422) * pi) }, { x: 0.9174044923141177, y: 0.8817466776391724, f: \x -> 0.8454621762175387 * sin (0.115257536467252 * (x + 0.03469723233181332) * pi) }, { x: 0.04039986415756336, y: 0.15271145439316458, f: \x -> 0.9490449699465466 * sin (0.6750180669000483 * (x + 0.23431970196414276) * pi) }, { x: 0.5659642215562013, y: 0.33905297640111876, f: \x -> 0.5954679109024937 * sin (0.32255562390725917 * (x + 0.4103045663404872) * pi) }, { x: 0.5932269683893593, y: 0.04137646988081334, f: \x -> 0.43123850944012776 * sin (0.3503950236984895 * (x + 0.8751854149669083) * pi) }, { x: 0.2600532049644848, y: 0.9512335323594845, f: \x -> 0.1330852815646522 * sin (0.11685915493811834 * (x + 0.8032567255861771) * pi) }, { x: 0.750596919302173, y: 0.5660647811529884, f: \x -> 0.12252811048084478 * sin (0.4288900646165352 * (x + 0.8220020230866586) * pi) }, { x: 0.7058481499412821, y: 0.33577510035321756, f: \x -> 0.9309011132534613 * sin (0.10807387415279282 * (x + 0.5466506710634887) * pi) }, { x: 0.7560511451536488, y: 0.06454735707577242, f: \x -> 0.43828083133250995 * sin (0.4445668204142387 * (x + 0.054743827468044626) * pi) }, { x: 0.6481088042227873, y: 0.6090098373024916, f: \x -> 0.14389170336038942 * sin (0.33867071677250293 * (x + 0.4469693618232342) * pi) }, { x: 0.36024818355657573, y: 0.44495171974439407, f: \x -> 0.9902656439613039 * sin (0.3127988834819777 * (x + 0.9987847933230615) * pi) }, { x: 0.4948827944750618, y: 0.6429648291847946, f: \x -> 0.4301864871375427 * sin (0.6723415663046644 * (x + 0.09093914204530151) * pi) }, { x: 0.19745107701800912, y: 0.7921651204622543, f: \x -> 0.9097728915806184 * sin (0.1963810981662855 * (x + 0.7245090758938467) * pi) }, { x: 0.11912897419443358, y: 0.03254896045113975, f: \x -> 0.22994027602254857 * sin (0.6939778711871162 * (x + 0.7388155499569684) * pi) }, { x: 0.24415494669411641, y: 0.472570097337776, f: \x -> 0.22902259190972618 * sin (0.615227866357919 * (x + 0.9890480993112479) * pi) }, { x: 0.9203841664465398, y: 0.6208633185102296, f: \x -> 0.3143331078045957 * sin (0.6815435814010501 * (x + 0.057924771636933325) * pi) }, { x: 0.9811499004861685, y: 0.5022262463844889, f: \x -> 0.21672054629593676 * sin (0.6210527552028506 * (x + 0.7557243610589263) * pi) }, { x: 0.9677634098982452, y: 0.3901345287041713, f: \x -> 0.43036366949289895 * sin (0.4233216192964807 * (x + 0.31944166044882005) * pi) }, { x: 0.5786073690858976, y: 0.586371361406231, f: \x -> 0.13646859604175726 * sin (0.12072088512536519 * (x + 0.5010522848830792) * pi) }, { x: 0.0012499448415448366, y: 0.12587572124009438, f: \x -> 0.18469053046531844 * sin (0.42390239395966556 * (x + 0.1531961573744779) * pi) }, { x: 0.16895243179895392, y: 0.5277320979707424, f: \x -> 0.865882371139905 * sin (0.5691280576641032 * (x + 0.3324058336640292) * pi) }, { x: 0.801460966627075, y: 0.45910383316395653, f: \x -> 0.6598164595121057 * sin (0.5260478420336041 * (x + 0.6697376194661933) * pi) }, { x: 0.8521339389706153, y: 0.3626348770849186, f: \x -> 0.7924312976020662 * sin (0.2991814837291323 * (x + 0.909971642527428) * pi) }, { x: 0.674812734540027, y: 0.7277770593790434, f: \x -> 0.6762131320540856 * sin (0.5681512855133772 * (x + 0.5556219498125824) * pi) }, { x: 0.6491557825332536, y: 0.07308319042126221, f: \x -> 0.4735695380926135 * sin (0.40780536241728604 * (x + 0.5349601344729933) * pi) }, { x: 0.42464225237190634, y: 0.7069760605800132, f: \x -> 0.535850587398882 * sin (0.2955149383605018 * (x + 0.20791069400385453) * pi) }, { x: 0.20088096344665818, y: 0.09158856466883636, f: \x -> 0.483227501295338 * sin (0.6709549705061462 * (x + 0.4560022670904379) * pi) }, { x: 0.3001051588250363, y: 0.33328222376853534, f: \x -> 0.9773099994432405 * sin (0.2698851893399844 * (x + 0.004287367099682582) * pi) }, { x: 0.7865828579399956, y: 0.49798726608825594, f: \x -> 0.4015876199547852 * sin (0.5042292671040394 * (x + 0.8001839080045724) * pi) }, { x: 0.5559852141603664, y: 0.7666693336871541, f: \x -> 0.647911241286775 * sin (0.3930000983877372 * (x + 0.28698956153301447) * pi) }, { x: 0.34500972739493296, y: 0.5973390313634365, f: \x -> 0.6587863792858866 * sin (0.46869827306319156 * (x + 0.15114144868234736) * pi) }, { x: 0.23374379075588625, y: 0.7583632900663713, f: \x -> 0.7400567456600466 * sin (0.29787747244782165 * (x + 0.6753439286421234) * pi) }, { x: 0.6873905620914865, y: 0.5451169574060583, f: \x -> 0.11367639049636262 * sin (0.3297740241276462 * (x + 0.020127227356116162) * pi) }, { x: 0.3355756682713278, y: 0.9359876560341573, f: \x -> 0.7549390556872413 * sin (0.1449320748833887 * (x + 0.739847533268351) * pi) }, { x: 0.6276724894939283, y: 0.5668762277782079, f: \x -> 0.2656702822304661 * sin (0.16571984171700352 * (x + 0.8723894908240698) * pi) }, { x: 0.22465866093491016, y: 0.7129778383867211, f: \x -> 0.3545569459686496 * sin (0.5742628607823999 * (x + 0.5172389520132499) * pi) }, { x: 0.8575345791452911, y: 0.47901213109080476, f: \x -> 0.40539337284147625 * sin (0.34845123795736366 * (x + 0.5940440256190052) * pi) }, { x: 0.020995471036586055, y: 0.33573817904636394, f: \x -> 0.5670171907199093 * sin (0.13614960442098947 * (x + 0.27164164393555656) * pi) } ]

star :: Number -> Number -> (Number -> Number) -> Number -> Number -> Number -> Drawing
star x y f w h t = let c = floor $ calcSlope (-1.0) 120.0 1.0 255.0 (f t) in filled (fillColor (rgb c c c)) (circle (x * w) (y * h) 1.0)

starFs = map (\{ x, y, f } -> star x y f) starDs :: Array (Number -> Number -> Number -> Drawing)
