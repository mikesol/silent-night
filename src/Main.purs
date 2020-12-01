module Klank.NatureBoy where

import Prelude
import Color (Color, rgb, rgba)
import Control.Parallel (parallel, sequential)
import Control.Promise (toAffE)
import Data.Array (catMaybes, filter, fold, head, range)
import Data.Either (Either, either)
import Data.Foldable (class Foldable, foldl, traverse_)
import Data.Int (toNumber)
import Data.Lens (_2, over)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (wrap)
import Data.NonEmpty (NonEmpty, (:|))
import Data.Profunctor (lcmap)
import Data.String (Pattern(..), indexOf)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Data.Typelevel.Num (D10, D2, D24, D3, D4, D5, D6)
import Data.Vec (Vec, empty, (+>))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, try)
import Effect.Exception (Error)
import Effect.Ref as Ref
import FRP.Behavior (Behavior, behavior)
import FRP.Behavior.Audio (AV(..), AudioContext, AudioParameter, BrowserAudioBuffer, CanvasInfo(..), Instruction, decodeAudioDataFromUri, defaultExporter, evalPiecewise, gain_, runInBrowser_, speaker')
import FRP.Event (Event, makeEvent, subscribe)
import Foreign.Object as O
import Graphics.Canvas (Rectangle)
import Graphics.Drawing (Drawing, Point, circle, fillColor, filled, rectangle, text)
import Graphics.Drawing.Font (FontOptions, bold, font, italic, sansSerif)
import Math (pow, (%))
import Record.Extra (SLProxy(..), SNil)
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

data Effects
  = Triangle (Vec D3 (Maybe Number))
  | Square (Vec D4 (Maybe Number))
  | Motion Point
  | Rise (Vec D6 (Tuple Number Boolean)) -- pos, stopped
  | Towards (Vec D4 (Either Point Number))
  | Explode ExplodeStage
  | Large (List (Tuple Point Number)) -- pos, startT
  | Bells (Vec D24 (Maybe Number))
  | Tether (Tuple Point Boolean) -- pos, holding
  | Gears (Vec D5 (Maybe Number))
  | Shrink (Vec D10 (Maybe Number))
  | Snow
  | NoEffect Number -- time

data Activity
  = Intro
  | HarmChooser { step :: HarmChooserStep }
  | SilentNightPlayer
    { verse :: Verse
    , verseOne :: VerseChoice
    , verseTwo :: VerseChoice
    , verseThree :: VerseChoice
    }

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

chooseVerseOne :: VerseChoice -> SilentNightAccumulator -> CanvasInfo -> Number -> Tuple SilentNightAccumulator Drawing
chooseVerseOne vc acc ci time = makeCanvas (acc { activity = HarmChooser { step: Row2Animation { startsAt: time, verseOne: vc } } }) ci time

chooseVerseTwo :: VerseChoice -> VerseChoice -> SilentNightAccumulator -> CanvasInfo -> Number -> Tuple SilentNightAccumulator Drawing
chooseVerseTwo v1 vc acc ci time = makeCanvas (acc { activity = HarmChooser { step: Row3Animation { startsAt: time, verseOne: v1, verseTwo: vc } } }) ci time

chooseVerseThree :: VerseChoice -> VerseChoice -> VerseChoice -> SilentNightAccumulator -> CanvasInfo -> Number -> Tuple SilentNightAccumulator Drawing
chooseVerseThree v1 v2 vc acc ci time = makeCanvas (acc { activity = HarmChooser { step: FadeOutAnimation { startsAt: time, verseOne: v1, verseTwo: v2, verseThree: vc } } }) ci time

doVAction :: SilentNightAccumulator -> Number -> Number -> VerseChoice -> Boolean
doVAction acc w h vc =
  let
    i = (toNumber <<< versionToInt) vc

    dim = ((w / circleDivisor) - (i * 5.0))

    halfDim = dim / 2.0
  in
    doAction acc
      { x: ((2.0 * i + 1.0) * w / 16.0) - halfDim
      , y: h - halfDim
      , width: dim
      , height: dim
      }

verseVersionChooser :: (VerseChoice -> SilentNightAccumulator -> CanvasInfo -> Number -> Tuple SilentNightAccumulator Drawing) -> Number -> Number -> SilentNightAccumulator -> CanvasInfo -> Number -> Tuple SilentNightAccumulator Drawing
verseVersionChooser vc w h acc ci time
  | doVAction acc w h VersionOne = vc VersionOne acc ci time
  | doVAction acc w h VersionTwo = vc VersionTwo acc ci time
  | doVAction acc w h VersionThree = vc VersionThree acc ci time
  | doVAction acc w h VersionFour = vc VersionFour acc ci time
  | doVAction acc w h VersionFive = vc VersionFive acc ci time
  | doVAction acc w h VersionSix = vc VersionSix acc ci time
  | doVAction acc w h VersionSeven = vc VersionSeven acc ci time
  | doVAction acc w h VersionEight = vc VersionSeven acc ci time
  | otherwise = Tuple acc (circles w h (const 1.0) 1.0)

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
    , curClickId :: Maybe Int
    , mousePosition :: Maybe { x :: Number, y :: Number }
    , activity :: Activity
    }

inRect :: Point -> Rectangle -> Boolean
inRect p r = p.x >= r.x && p.y >= r.y && p.x <= (r.x + r.width) && p.y <= (r.y + r.height)

doAction :: SilentNightAccumulator -> Rectangle -> Boolean
doAction acc r = acc.initiatedClick && (maybe false (flip inRect r) acc.mousePosition)

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

circleFan = 2.0 :: Number

circleFade = 0.5 :: Number

circleFlyAway = 3.0 :: Number

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

circleDivisor = 9.0 :: Number

whiteRGBA = rgba 255 255 255 :: Number -> Color

circles :: Number -> Number -> (VerseChoice -> Number) -> Number -> Drawing
circles w h opq traj = fold (map (\i' -> let i = (toNumber <<< versionToInt) i' in filled (fillColor (whiteRGBA (opq i'))) (circle ((2.0 * i + 1.0) * w * traj / 16.0) h ((w / circleDivisor) - (i * 5.0)))) verseChoices)

circleFanner :: Number -> Number -> Number -> Number -> Drawing
circleFanner w h startsAt time = circles w h (const $ min 1.0 ((time - startsAt) / circleFade)) (min 1.0 ((time - startsAt) / circleFan))

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

    (Tuple x1 y1) = circleOutroStart w h vs vc

    x = calcSlope 0.0 x0 circleFlyAway x1 (time - startsAt)

    y = calcSlope 0.0 y0 circleFlyAway y1 (time - startsAt)
  in
    filled (fillColor (whiteRGBA opq)) (circle x y ((w / circleDivisor) - (i * 5.0)))

makeCanvas :: SilentNightAccumulator -> CanvasInfo -> Number -> (Tuple SilentNightAccumulator Drawing)
makeCanvas acc ci@(CanvasInfo { w, h }) time = over _2 (append (filled (fillColor (rgb 0 0 0)) (rectangle 0.0 0.0 w h))) go
  where
  go = case acc.activity of
    Intro ->
      if time > instructionFadeOut then
        makeCanvas (acc { activity = HarmChooser { step: Row1Animation { startsAt: time } } }) ci time
      else
        Tuple acc
          ( text
              ( font sansSerif
                  (if time > silentNightDark then 20 else 48)
                  (if time > silentNightDark then mempty else bold)
              )
              (w / 2.0)
              (h / 2.0)
              (fillColor (whiteRGBA (introOpacity time)))
              if time > silentNightDark then "Click on or press three circles" else "Silent Night"
          )
    HarmChooser { step } -> case step of
      Row1Animation i ->
        if time > i.startsAt + circleFan then
          makeCanvas (acc { activity = HarmChooser { step: Row1Choose } }) ci time
        else
          Tuple acc (circleFanner w (firstRow h) i.startsAt time)
      Row1Choose -> verseVersionChooser chooseVerseOne w (firstRow h) acc ci time
      Row2Animation i ->
        if time > i.startsAt + circleFan then
          makeCanvas (acc { activity = HarmChooser { step: Row2Choose { verseOne: i.verseOne } } }) ci time
        else
          Tuple acc (circleFanner w (secondRow h) i.startsAt time <> circleChoice w (firstRow h) i.verseOne i.startsAt time)
      Row2Choose i -> verseVersionChooser (chooseVerseTwo i.verseOne) w (secondRow h) acc ci time
      Row3Animation i ->
        if time > i.startsAt + circleFan then
          makeCanvas (acc { activity = HarmChooser { step: Row3Choose { verseOne: i.verseOne, verseTwo: i.verseTwo } } }) ci time
        else
          Tuple acc (circleFanner w (thirdRow h) i.startsAt time <> circleChoice w (secondRow h) i.verseOne i.startsAt time <> circleChosen w (firstRow h) i.verseTwo)
      Row3Choose i -> verseVersionChooser (chooseVerseThree i.verseOne i.verseTwo) w (thirdRow h) acc ci time
      FadeOutAnimation i ->
        if time > i.startsAt + circleFade then
          makeCanvas (acc { activity = SilentNightPlayer { verse: Verse1, verseOne: i.verseOne, verseTwo: i.verseTwo, verseThree: i.verseThree } }) ci time
        else
          Tuple acc
            ( fold
                ( map (\vc -> circleOutro Verse1 vc (vc == i.verseOne) w h i.startsAt time) verseChoices
                    <> map (\vc -> circleOutro Verse2 vc (vc == i.verseTwo) w h i.startsAt time) verseChoices
                    <> map (\vc -> circleOutro Verse3 vc (vc == i.verseThree) w h i.startsAt time) verseChoices
                )
            )
    _ -> Tuple acc mempty

scene :: Interactions -> SilentNightAccumulator -> CanvasInfo -> Number -> Behavior (AV D2 SilentNightAccumulator)
scene inter acc' ci'@(CanvasInfo ci) time = go <$> (interactionLog inter)
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
            <$> head p
        , initiatedClick = (_.id <$> head p) /= acc'.curClickId
        , curClickId = _.id <$> head p
        }

    (Tuple vizAcc cvs) = makeCanvas acc ci' time

    players = Nil

main :: Klank' SilentNightAccumulator
main =
  klank
    { run =
      runInBrowser_ do
        inter <- getInteractivity
        pure $ scene inter
    , accumulator =
      \res _ ->
        res
          { initiatedClick: false
          , curClickId: Nothing
          , mousePosition: Nothing
          , activity: Intro
          }
    , exporter = defaultExporter
    , buffers =
      makeBuffersKeepingCache
        []
    }

newtype Interactions
  = Interactions
  { interactions :: Ref.Ref (InteractionOnsets)
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
  interactions <- Ref.new []
  target <- toEventTarget <$> window
  touchStartListener <-
    eventListener \e -> do
      fromEvent e
        # traverse_ \me -> do
            nt <- Ref.modify (_ + 1) nInteractions
            handleTE nt interactions me
  mouseStartListener <-
    eventListener \e -> do
      ME.fromEvent e
        # traverse_ \me -> do
            nt <- Ref.modify (_ + 1) nInteractions
            handleME nt interactions me
  if mobile then addEventListener (wrap "touchstart") touchStartListener false target else addEventListener (wrap "mousedown") mouseStartListener false target
  let
    dispose =
      if mobile then do
        removeEventListener (wrap "touchstart") touchStartListener false target
      else do
        removeEventListener (wrap "mousedown") mouseStartListener false target
  pure (Interactions { interactions, dispose })

withInteractions ::
  forall a.
  Interactions ->
  Event a ->
  Event { value :: a, interactions :: InteractionOnsets }
withInteractions (Interactions { interactions }) e =
  makeEvent \k ->
    e
      `subscribe`
        \value -> do
          interactionsValue <- Ref.read interactions
          k { value, interactions: interactionsValue }

interactionLog :: Interactions -> Behavior (InteractionOnsets)
interactionLog m = behavior \e -> map (\{ value, interactions: bs } -> value bs) (withInteractions m e)
