{-# LANGUAGE TypeFamilies #-}
import Data.Fixed (mod', Pico)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Prelude hiding (truncate)
import Data.Maybe (isNothing, fromJust)
import Distribution.Compat.Time (getCurTime)
import Control.Arrow (ArrowChoice(right))

type Number = Double

-- [[ ------------------------ Time Constants -------------------------- ]] --
-- [[ ------------------------------------------------------------------ ]] --

minuteDuration :: NominalDiffTime
minuteDuration = 60

hourDuration :: NominalDiffTime
hourDuration = 60 * minuteDuration

dayDuration :: NominalDiffTime
dayDuration = 24 * hourDuration


-- [[ -------------------------- Astrolabe ----------------------------- ]] --
-- [[ ------------------------------------------------------------------ ]] --

-- Constants are gained from constructing the Astrolabe in Geogebra

cancerTropic :: Circle
cancerTropic = Circle originPoint 1

equator :: Circle
equator = Circle originPoint 0.6556

ariesTropic :: Circle
ariesTropic = Circle originPoint 4.298

-- Horizon and twilight lines are dependent on the astrolabe's latitude
-- These value are given by latitude of the Prague Astronomical Clock (50.0871°)

horizonLine :: Circle
horizonLine = Circle (Vec2 0.0 (-0.7038)) 1.0219

twilightLine :: Circle
twilightLine = Circle (Vec2 0.0 (-0.5385)) 0.7083

zodiacRadius :: Number
zodiacRadius = 0.7149

zodiacDistanceFromOrigin :: Number
zodiacDistanceFromOrigin = 0.2851


-- [[ ---------------------------- Gears ------------------------------- ]] --
-- [[ ------------------------------------------------------------------ ]] --

-- The hearts of clocks are mechanical gears, clock hands are attached to these gears
-- Each of them makes a full revolution in specific amount of time,
--      making it easy to calculate angular speed

-- Time at which a gear is in its default poistion is called an epoch
-- Usually its a time when the gear's hand points directly upwards.
-- The angle the gear is rotated then represents how much time has passed since the epoch

-- Gear (revolution duration, epoch)
data Gear = Gear NominalDiffTime UTCTime

angularSpeed :: Gear -> Angle
angularSpeed (Gear revolution _) = fullAngle ./ realToFrac revolution

-- Returns angular distance (radians) the gear travels in given time
rotationAfterTime :: Gear -> NominalDiffTime -> Angle
rotationAfterTime gear time = realToFrac time .* angularSpeed gear

-- Returns the angle the gear makes with its default position at epoch
angleAtTime :: Gear -> UTCTime -> Angle
angleAtTime gear utc =
    let (Gear _ epoch) = gear
    in  rotationAfterTime gear $ diffUTCTime utc epoch


-- + -------------------------------------------------------------------- + --

sunGear :: Gear
sunGear = Gear duration epoch
    where duration = dayDuration
          epoch    = UTCTime (fromGregorian 2000 6 21) (realToFrac $ 11 * hourDuration)

zodiacGear :: Gear
zodiacGear = Gear duration epoch
    where duration = 365 / 366 * dayDuration
          epoch    = UTCTime (fromGregorian 2000 6 21) (realToFrac $ 11 * hourDuration)

moonGear :: Gear
moonGear = Gear duration epoch
    where duration = 378.83 / 366 * dayDuration
          epoch    = UTCTime (fromGregorian 2000 6 21) (realToFrac $ 3 * hourDuration)

-- Represents one moon revolution around its axis, epoch is at full moon
moonPhaseGear :: Gear
moonPhaseGear = Gear duration epoch
    where duration = 29.5305 * dayDuration
          epoch    = UTCTime (fromGregorian 2000 12 11) (realToFrac $ 10 * hourDuration + 2 * minuteDuration)

calendarGear :: Gear
calendarGear = Gear duration epoch
    where duration = 365 * dayDuration
          epoch    = UTCTime (fromGregorian 2000 1 1) 0


-- [[ --------------------------- Clocks ------------------------------- ]] --
-- [[ ------------------------------------------------------------------ ]] --

class Clock c where
    type ClockValue c
    fromUtc :: c -> UTCTime -> ClockValue c

now :: Clock c => c -> IO (ClockValue c)
now clock = do
    fromUtc clock <$> getCurrentTime

inTime :: Clock c => c -> NominalDiffTime -> IO (ClockValue c)
inTime clock time = do
    fromUtc clock . addUTCTime time <$> getCurrentTime


-- + -------------------------------------------------------------------- + --

-- Central European Time, UTC +1
-- Shown in Roman numerals from I to XII on the inner dial, pointed at by the hand

data CETClock = CETClock
instance Clock CETClock where
    type ClockValue CETClock = CETClockValue

    fromUtc :: CETClock -> UTCTime -> CETClockValue
    fromUtc _ utc = CETClockVal hour
        where unit = straightAngle ./ 12
              hour = toEnum $ floor $ (angleAtTime sunGear utc / unit) `mod'` 12


newtype CETClockValue = CETClockVal RomanNumerals
instance Show CETClockValue where
    show (CETClockVal val) = "CET Clock -> " ++ show val


-- + -------------------------------------------------------------------- + --

-- Old Czech Time, "https://cs.wikipedia.org/wiki/Vlašské_hodiny"

-- Divides day into 24 equal parts, beginning at the sunset
-- Shown in Gothic numerals from 1 to 24 on the inner dial, pointed at by the hand

data OldCzechClock = OldCzechClock

instance Clock OldCzechClock where
    type ClockValue OldCzechClock = OldCzechClockValue

    fromUtc :: OldCzechClock -> UTCTime -> OldCzechClockValue
    fromUtc _ utc = OldCzechClockVal hour1to24
        where (UTCTime day time) = utc
              unit = fullAngle ./ 24
              todaysSunset = timeOfDayToTime $ sunsetTime day
              lastSunsetAzimuth = if time > todaysSunset then sunsetAzimuth day
                                  else sunsetAzimuth (addDays (-1) day)

              hour0to23 = floor $ (sunAzimuth utc .- lastSunsetAzimuth) / unit
              hour1to24 = if hour0to23 == 0 then 24
                          else hour0to23


newtype OldCzechClockValue = OldCzechClockVal Int
instance Show OldCzechClockValue where
    show (OldCzechClockVal val) = "Old Czech Time -> " ++ show val


-- + -------------------------------------------------------------------- + --

-- Sidereal time, https://en.wikipedia.org/wiki/Sidereal_time

-- Defines day as time it takes for Earth to make one full revolution around its axis, which is 23h 56m 4s
-- In the span of year, the difference between these times makes a full day

-- Shown in Roman numerals from I to XII on the inner dial, pointed by the little star
-- It revolves at the same rate as the zodiac gear

data SiderealClock = SiderealClock
instance Clock SiderealClock where
    type ClockValue SiderealClock = SiderealClockValue

    fromUtc :: SiderealClock -> UTCTime -> ClockValue SiderealClock
    fromUtc _ utc = SiderealClockVal hour
        where unit = straightAngle ./ 12
              starHandAngle = angleAtTime zodiacGear utc .+ rightAngle   -- At epoch, star points directly eastwards
              hour = toEnum $ floor $ (starHandAngle / unit) `mod'` 12

newtype SiderealClockValue = SiderealClockVal RomanNumerals
instance Show SiderealClockValue where
    show (SiderealClockVal val) = "Sidereal time -+ " ++ show val


-- + -------------------------------------------------------------------- + --

-- Babylonian time (or Planetary hours), https://en.wikipedia.org/wiki/Planetary_hours#Table_of_hours

-- Devides day into 12 equal parts, starting at sunrise and ending at sunrise
-- During night the time is not shown on the clock

-- Shown in Arabian numerals from 1 to 12 in golden sections of the inner dial,
--   told by the symbol of Sun lying in a certain section

data BabylonianClock = BabylonianClock
instance Clock BabylonianClock where
    type ClockValue BabylonianClock = BabylonianClockValue

    fromUtc :: BabylonianClock -> UTCTime -> ClockValue BabylonianClock
    fromUtc _ utc = if isNight then BabylonianNight
                    else BabylonianClockVal hour
        where (UTCTime day time) = utc
              unit = (sunsetAzimuth day .- sunriseAzimuth day) ./ 12
              isNight = time > timeOfDayToTime (sunsetTime day) ||
                        time < timeOfDayToTime (sunriseTime day)

              hour = floor $ (sunAzimuth utc .- sunriseAzimuth day) / unit + 1


data BabylonianClockValue = BabylonianClockVal Int | BabylonianNight
instance Show BabylonianClockValue where
    show BabylonianNight = "Babylonian clock -* Night"
    show (BabylonianClockVal hour) = "Babylonian clock -* " ++ show hour


-- + -------------------------------------------------------------------- + --

-- Moon Phase
-- Shown on the small moon model on the moon hand,
-- The model of moon on the moon hand is actually half-white, half-black,
--   and revolves around its axis in the opposite direction of the hand

data MoonPhase = MoonPhase
instance Clock MoonPhase where
    type ClockValue MoonPhase = MoonPhaseValue

    fromUtc :: MoonPhase -> UTCTime -> MoonPhaseValue
    fromUtc _ utc =
        let gearAngle = angleAtTime moonPhaseGear utc
            side = if sin gearAngle > 0 then L
                   else R
            portion = (cos gearAngle + 1) / 2
        in  MoonPhaseVal side portion


-- MoonPhaseVal (side from which the moon is illuminated, portion of the moon that is illuminated <0;1>)
data MoonPhaseValue = MoonPhaseVal LeftRight Number
instance Show MoonPhaseValue where
    show (MoonPhaseVal side portion) =
        let percentage = take 5 (show $ portion * 100) ++ "%"
            base = "Moon Phase: " ++ show side ++ " " ++ percentage
            inQuarter = portion > 0.45 && portion < 0.55
        in  if portion > 0.95 then base ++ " (Full moon)"
            else if inQuarter && side == R then base ++ " (First Quarter)"
            else if inQuarter && side == L then base ++ " (Last Quarter)"
            else if portion < 0.05 then base ++ " (New Moon)"
            else base


data LeftRight = L | R deriving (Enum, Show, Eq)

-- + -------------------------------------------------------------------- + --

-- Not really a clock, but a collection of interesting informations about position of Sun and Moon

newtype CelestialPosition = CelestialPosition {
    celestialBody :: CelestialBody
}

instance Clock CelestialPosition where
    type ClockValue CelestialPosition = CelestialValues

    fromUtc this utc = CelestialVals body azimuth occluded altitude zodiac rise set
        where body = celestialBody this
              (UTCTime day _) = utc

              azimuth = celestialAzimuth body utc
              occluded = occlusion body utc
              altitude = undefined
              zodiac = undefined
              rise = timeAtRise body day
              set = timeAtSet body day 


data CelestialValues = CelestialVals {
    clocksCelestialBody :: CelestialBody,
    azimuth :: Angle,
    occluded :: Occlusion,
    altitude :: Angle,
    zodiac :: ZodiacSign,
    rise :: TimeOfDay,
    set :: TimeOfDay
}

instance Show CelestialValues where
    show vals = "Celestial positions of " ++  show (clocksCelestialBody vals) ++ "\n" ++
                "Azimuth: " ++ take 5 (show $ toDegrees (azimuth vals)) ++ "° " ++ 
                    show (occluded vals) ++ "\n" ++
                "Time of rise: " ++ take 8 (show $ rise vals) ++ "\n" ++
                "Time of set: " ++ take 8 (show $ set vals)


-- + -------------------------------------------------------------------- + --

-- Astronomical clock, combination of all the previous clocks

data AstronomicalClock = AstronomicalClock
instance Clock AstronomicalClock where
    type ClockValue AstronomicalClock = AstronomicalClockValues

    fromUtc _ utc = AstronomicalClockVals
        (fromUtc CETClock utc)
        (fromUtc OldCzechClock utc)
        (fromUtc SiderealClock utc)
        (fromUtc BabylonianClock utc)
        (fromUtc MoonPhase utc)
        (fromUtc (CelestialPosition Sun) utc)
        (fromUtc (CelestialPosition Moon) utc)

data AstronomicalClockValues = AstronomicalClockVals {
    acCET :: CETClockValue,
    acOldCzech :: OldCzechClockValue,
    acSidereal :: SiderealClockValue,
    acBabylonian :: BabylonianClockValue,
    acMoonPhase :: MoonPhaseValue,
    acSun :: CelestialValues,
    acMoon :: CelestialValues
}

instance Show AstronomicalClockValues where
    show vals = "\n==[[======= Prague Astronomical Clock =======]]==" ++ "\n" ++
                show (acCET vals) ++ "\n" ++
                show (acOldCzech vals) ++ "\n" ++
                show (acSidereal vals) ++ "\n" ++
                show (acBabylonian vals) ++ "\n" ++
                "-- + --------------------------------------- + --" ++ "\n" ++
                show (acMoonPhase vals) ++ "\n" ++ 
                "-- + --------------------------------------- + --" ++ "\n" ++
                show (acSun vals) ++ "\n" ++
                "-- + --------------------------------------- + --" ++ "\n" ++
                show (acMoon vals) ++ "\n" ++
                "=================================================" ++ "\n"


data RomanNumerals = XII | I | II | III | IV | V | VI | VII | VIII | IX | X | XI deriving (Enum, Show)

-- [[ -------------------------- Astronomy ----------------------------- ]] --
-- [[ ------------------------------------------------------------------ ]] --

-- Sun and Moon share calculation for their position on their celestial sphere
-- The only difference is the speed at which they revolve around the Earth
-- This speed is given by the gear they sit on

data CelestialBody = Sun | Moon deriving (Show)

celestialGear :: CelestialBody -> Gear
celestialGear Sun = sunGear
celestialGear Moon = moonGear

-- Azimuth is the angle the celestial body makes with the zenith.
-- Zenith is the direction perpendicular to ground, sun reaches zenith at noon
celestialAzimuth :: CelestialBody -> UTCTime -> Angle
celestialAzimuth body = angleAtTime $ celestialGear body

positionOnDial :: CelestialBody -> UTCTime -> Vec2
positionOnDial body utc =
    let hand = Ray originPoint (unitVector $ celestialAzimuth body utc)
        zodiac = zodiacCircle utc
    in  fst $ fromJust $ rayCircleIntersection hand zodiac

azimuthAtSet :: CelestialBody -> Day -> Angle
azimuthAtSet body day =
    let position = positionOnDial body (UTCTime day 0)
        orbit = Circle originPoint (magnitude position)
        (Vec2 x1 y1, Vec2 x2 y2) = fromJust $ circlesIntersection orbit horizonLine
    in  if x1 > x2 then vecAzimuth (Vec2 x1 y1)
        else vecAzimuth (Vec2 x2 y2)

azimuthAtRise :: CelestialBody -> Day -> Angle
azimuthAtRise body day = negateAngle $ azimuthAtSet body day

timeAtSet :: CelestialBody -> Day -> TimeOfDay
timeAtSet body day =
    let azimuth = azimuthAtSet body day
        unit = fullAngle ./ 24
        hours = (azimuth / unit) + 11                   -- since azimuth 0 represents noon in CET
    in  timeToTimeOfDay (realToFrac hours * realToFrac hourDuration)


timeAtRise :: CelestialBody -> Day -> TimeOfDay
timeAtRise body day =
    let sunset = timeOfDayToTime $ timeAtSet body day
        noon = 11 * realToFrac hourDuration
        timeFromNoon = sunset - noon
    in  timeToTimeOfDay $ noon - timeFromNoon           -- set and rise lies at equal distance from noon on the dial


-- + -------------------------------------------------------------------- + --

-- Colors on the inner dial represent how much is celestial body occluded by the Earth's horizon
-- Both Sun and Moon travel through all of its parts during day, 
--      but its easier to describe with the position of the Sun

-- Blue part of the dial represents day, when the sun is clearly visible on the sky
-- Orange part represents sunrise (AVRORA) and sunset (CREPASCVS), and is bounded by the horizon line
-- Just above the horizon line is dawn (ORTVS) and twilight (OCCASVS),
--      at the same position as 1 and 12 of the Babylonian clock

-- 18 Degrees below the the horizon line is situated the black part of the dial,
--      it represents astronomical night, time at which the stars are visible the most

data Occlusion = Day | Night | AVRORA | ORTVS | OCCASVS | CREPASCVS deriving (Enum, Eq)

occlusion :: CelestialBody -> UTCTime -> Occlusion
occlusion body utc
    |  between azimuth (set .+ nightAngle, rise .- nightAngle) = Night
    |  between azimuth (rise .- nightAngle, rise)              = AVRORA
    |  between azimuth (set, set .+ nightAngle)                = CREPASCVS
    |  between azimuth (rise, rise .+ twilightAngle)           = ORTVS
    |  between azimuth (set .- twilightAngle, set)             = OCCASVS
    |  otherwise                                               = Day
    where (UTCTime day _) = utc
          azimuth = celestialAzimuth body utc
          rise = azimuthAtRise body day
          set = azimuthAtSet body day

          nightAngle = fromDegrees 18
          twilightAngle = (set .- rise) / 12

instance Show Occlusion where 
    show Night = "(Astronomical Night)"
    show AVRORA = "(AVRORA)"
    show ORTVS = "(ORTVS)"
    show OCCASVS = "(OCCASVS)"
    show CREPASCVS = "(CREPASCVS)"
    show Day = ""

-- + -------------------------------------------------------------------- + --

data ZodiacSign = Cancer | Leo | Libra | Virgo | Scorpio | Sagittarius | 
                  Capricorn | Aquarius | Pisces | Aries | Taurus | Gemini
                  deriving (Enum, Show)

-- Creates circle representing the zodiac dial at given time
zodiacCircle :: UTCTime -> Circle
zodiacCircle utc = Circle center zodiacRadius
    where gearAzimuth = angleAtTime zodiacGear utc
          center = scaleVector zodiacDistanceFromOrigin (unitVector gearAzimuth)


-- + -------------------------------------------------------------------- + --

today :: IO Day
today = do
    (UTCTime day _) <- getCurrentTime
    return day

inDays :: Int -> IO Day
inDays days = do
    (UTCTime day _) <- addUTCTime (fromIntegral days * dayDuration) <$> getCurrentTime
    return day


-- + -------------------------------------------------------------------- + --

-- Convenience functions

sunAzimuth = celestialAzimuth Sun
moonAzimuth = celestialAzimuth Moon

sunsetAzimuth = azimuthAtSet Sun
sunriseAzimuth = azimuthAtRise Sun
moonsetAzimuth = azimuthAtSet Moon
moonriseAzimuth = azimuthAtRise Moon

sunsetTime = timeAtSet Sun
sunriseTime = timeAtRise Sun
moonsetTime = timeAtSet Moon
moonriseTime = timeAtRise Moon


-- [[ --------------------------- Geometry ----------------------------- ]] --
-- [[ ------------------------------------------------------------------ ]] --

-- Alias for angle in radians
type Angle = Number

fullAngle :: Angle
fullAngle = 2 * pi

straightAngle :: Angle
straightAngle = pi

rightAngle :: Angle
rightAngle = pi / 2

-- Angle additian
infixl 6 .+
(.+) :: Angle -> Angle -> Angle
a .+ b = (a + b) `mod'` fullAngle

oppositeAngle :: Angle -> Angle
oppositeAngle a = a .+ straightAngle

negateAngle :: Angle -> Angle
negateAngle a = fullAngle - a `mod'` fullAngle

-- Angle subtraction
infixl 6 .-
(.-) :: Angle -> Angle -> Angle
a .- b =  a .+ negateAngle b

-- Angle multiplication
infixl 7 .*
(.*) :: Number -> Angle -> Angle
a .* b = (a * b) `mod'` fullAngle

-- Angle division
infixl 7 ./
(./) :: Angle -> Number -> Angle
a ./ b = a .* (1 / b)

fromDegrees :: Number -> Angle
fromDegrees degs = degs / 180 * pi

toDegrees :: Angle -> Number
toDegrees rads = rads / pi * 180


-- + -------------------------------------------------------------------- + --

-- 2D Vector (x coordinate, y coordinate)
data Vec2 = Vec2 Number Number deriving (Eq)

addVectors :: Vec2 -> Vec2 -> Vec2
addVectors (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

negVector :: Vec2 -> Vec2
negVector (Vec2 x y) = Vec2 (-x) (-y)

subVectors :: Vec2 -> Vec2 -> Vec2
subVectors v1 v2 = addVectors v1 (negVector v2)

scaleVector :: Number -> Vec2 -> Vec2
scaleVector t (Vec2 x y) = Vec2 (t * x) (t * y)

dot :: Vec2 -> Vec2 -> Number
dot (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2

magnitude :: Vec2 -> Number
magnitude (Vec2 x y) = pythagorean x y

-- Returns angle in radians, as if the point was in polar coordinates
vecAzimuth :: Vec2 -> Angle
vecAzimuth (Vec2 x y) = (- atan2 y x) .+ rightAngle

normalized :: Vec2 -> Vec2
normalized v = scaleVector (1 / magnitude v) v

-- Returns vector of magnitude 1, with azimuth given in radians
unitVector :: Angle -> Vec2
unitVector azimuth = Vec2 (cos theta) (sin theta)
    where theta = negateAngle azimuth .+ rightAngle

originPoint :: Vec2
originPoint = Vec2 0 0


instance Show Vec2 where
    show (Vec2 x y) = "[" ++ show x ++ ", " ++ show y ++ "]"

-- + -------------------------------------------------------------------- + --

-- Ray (ray origin, direction unit Vector)
data Ray = Ray Vec2 Vec2

-- Return point lying on ray at specified parameter t
rayPointAt :: Ray -> Number -> Vec2
rayPointAt (Ray origin direction) t = addVectors origin (scaleVector t direction)


-- + -------------------------------------------------------------------- + --

-- Circle (center, radius)
data Circle = Circle Vec2 Number
instance Show Circle where
    show (Circle center radius) = "Circle " ++ show center ++ " Radius: " ++ show radius

-- Returns intersections of ray and circle if they exist, return Nothing if not
-- First point is closer to the ray origin, second is further.
rayCircleIntersection :: Ray -> Circle -> Maybe (Vec2, Maybe Vec2)
rayCircleIntersection ray (Circle center radius) =
    let (Ray origin direction) = ray
        shiftVec = subVectors origin center
        a = dot direction direction
        b = 2 * dot shiftVec direction
        c = dot shiftVec shiftVec - radius^2
        roots = quadraticFormula a b c

        points Nothing = Nothing
        points (Just (root1, root2)) =
            let closer = min root1 root2
                further = max root1 root2
            in  if further < 0 then Nothing
                else if closer < 0 then Just (rayPointAt ray further, Nothing)
                else if closer == further then Just (rayPointAt ray closer, Nothing)
                else Just (rayPointAt ray closer, Just $ rayPointAt ray further)

    in  points roots


-- Returns intersections of 2 circles if they exist, return Nothing if not
-- Points are not in any particular order, if circles intersect in only one point, both results will be the same
circlesIntersection :: Circle -> Circle -> Maybe (Vec2, Vec2)
circlesIntersection (Circle (Vec2 x1 y1) r1) (Circle (Vec2 x2 y2) r2)
    | Vec2 x1 y1 == Vec2 x2 y2 = Nothing        -- Circles have the same center
    | x1 == x2 =
        let c = (- x1^2 + x2^2 - y1^2 + y2^2 + r1^2 - r2^2) / (2 * (y2 - y1)) - y1
            d = (x2 - x1) / (y2 - y1)

            xRoots = quadraticFormula (d^2 + 1) (-2 * c * d - 2 * x1) (-r1^2 + c^2 + x1^2)
            yFromX x = (c + y1) - d * x

        in  if isNothing xRoots then Nothing
            else Just $ mapTuple (\ x -> Vec2 x (yFromX x)) (fromJust xRoots)

    | otherwise =
        let c = (- x1^2 + x2^2 - y1^2 + y2^2 + r1^2 - r2^2) / (2 * (x2 - x1)) - x1
            d = (y2 - y1) / (x2 - x1)

            yRoots = quadraticFormula (d^2 + 1) (-2 * c * d - 2 * y1) (-r1^2 + c^2 + y1^2)
            xFromY y = (c + x1) - d * y

        in  if isNothing yRoots then Nothing
            else Just $ mapTuple (\ y -> Vec2 (xFromY y) y) (fromJust yRoots)

-- + -------------------------------------------------------------------- + --

-- Solves x for ax^2 + bx + c = 0
-- Returnes both solutions if they exist, returns Nothing if none exist
quadraticFormula :: Number -> Number -> Number -> Maybe (Number, Number)
quadraticFormula a b c =
    let d = b^2 - 4 * a * c
    in  if d < 0 then Nothing
        else if a == 0 then Just ((-c) / b, (-c) /b)
        else Just ((-b + sqrt d) / (2 * a),
                   (-b - sqrt d) / (2 * a))

-- Computes the length of hypotenuse from the Pythagorean theorem
pythagorean :: Number -> Number -> Number
pythagorean a b = sqrt (a^2 + b^2)


-- + -------------------------------------------------------------------- + --

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)


between :: Ord n => n -> (n, n) -> Bool
between val (lower, upper) = val > lower && val < upper