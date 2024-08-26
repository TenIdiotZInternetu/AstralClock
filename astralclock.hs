{-# LANGUAGE TypeFamilies #-}
import Data.Fixed (mod', Pico)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Prelude hiding (truncate)
import Data.Maybe (isNothing, fromJust)
import Distribution.Compat.Time (getCurTime)

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
horizonLine = Circle (Vec2 0.0 (-0.7838)) 1.0219

twilightLine :: Circle
twilightLine = Circle (Vec2 0.0 (-0.5385)) 0.7083

zodiacRadius :: Float
zodiacRadius = 0.7149

zodiacDistanceFromOrigin :: Float
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

angularSpeed :: Gear -> Float
angularSpeed (Gear revolution _) = realToFrac (2 * pi) / realToFrac revolution

-- Returns angular distance (radians) the gear travels in given time
rotationAfterTime :: Gear -> NominalDiffTime -> Float
rotationAfterTime gear time = realToFrac time * angularSpeed gear

-- Returns the angle the gear makes with its default position at epoch
angleAtTime :: Gear -> UTCTime -> Float
angleAtTime gear utc =
    let (Gear _ epoch) = gear
        distanceTraveled = rotationAfterTime gear $ diffUTCTime utc epoch
    in  distanceTraveled `mod'` (2 * pi)


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
        where unit = pi / 12
              hour = toEnum $ floor $ (angleAtTime sunGear utc / unit) `mod'` 12


newtype CETClockValue = CETClockVal RomanNumerals
instance Show CETClockValue where
    show (CETClockVal val) = "CET Clock -> " ++ show val


-- + -------------------------------------------------------------------- + --

-- Old Czech Time, "https://cs.wikipedia.org/wiki/Vlašské_hodiny"

-- Divides day in 24 equal parts, beginning at the sunset
-- Shown in Gothic numerals from 1 to 24 on the inner dial, pointed at by the hand

-- data OldCzechClock = OldCzechClock

-- instance Clock OldCzechClock where
--     type ClockValue OldCzechClock = OldCzechClockValue

--     fromUtc :: OldCzechClock -> UTCTime -> OldCzechClockValue
--     fromUtc _ utc = OldCzechClockVal hour
--         where unit = pi / 12
--               sunGearAngle = angleAtTime sunGear utc
--               hour = floor $ (sunGearAngle - sunsetAzimuth) / unit


-- newtype OldCzechClockValue = OldCzechClockVal Int
-- instance Show OldCzechClockValue where
--     show (OldCzechClockVal val) = "Old Czech Time -> " ++ show val


-- + -------------------------------------------------------------------- + --

-- Sidereal time, https://en.wikipedia.org/wiki/Sidereal_time

-- Defines day as time it takes for Earth to make one full revolution around its axis, which is 23h 56m 4s
-- In the span of year, the difference between these times makes a full day

-- Shown in Roman numerals from I to XII on the inner dial, pointed by the little star
-- It revolves at the same rate as the zodiac gear

data SiderealClock = SiderealClock

instance Clock SiderealClock where
    type ClockValue SiderealClock = SiderealClockValue

    fromUtc _ utc = SiderealClockVal hour
        where unit = pi / 12
              starHandAngle = angleAtTime zodiacGear utc + pi / 2   -- At epoch, star points directly eastwards
              hour = toEnum $ floor $ (starHandAngle / unit) `mod'` 12

newtype SiderealClockValue = SiderealClockVal RomanNumerals
instance Show SiderealClockValue where
    show (SiderealClockVal val) = "Sidereal time -+ " ++ show val


-- + -------------------------------------------------------------------- + --

-- Moon Phase
-- Shown on the small moon model on the moon hand,
-- The hand is actually half-white, half-black, and revolves around its axis in the direction of the hand

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
data MoonPhaseValue = MoonPhaseVal LeftRight Float
instance Show MoonPhaseValue where
    show (MoonPhaseVal side portion) =
        let percentage = show (portion * 100) ++ "%"
            base = "Moon Phase: " ++ show side ++ " " ++ percentage
            inQuarter = portion > 0.45 && portion < 0.55
        in  if portion > 0.95 then base ++ " (Full moon)"
            else if inQuarter && side == R then base ++ " (First Quarter)"
            else if inQuarter && side == L then base ++ " (Last Quarter)"
            else if portion < 0.05 then base ++ " (New Moon)"
            else base


data LeftRight = L | R deriving (Enum, Show, Eq)

-- + -------------------------------------------------------------------- + --


data RomanNumerals = I | II | III | IV | V | VI | VII | VIII | IX | X | XI | XII deriving (Enum, Show)


-- [[ -------------------------- Astronomy ----------------------------- ]] --
-- [[ ------------------------------------------------------------------ ]] --

today :: IO Day
today = do
    (UTCTime day _) <- getCurrentTime
    return day

inDays :: Int -> IO Day
inDays days = do
    (UTCTime day _) <- addUTCTime (fromIntegral days * dayDuration) <$> getCurrentTime
    return day

sunsetTime :: Day -> UTCTime
sunsetTime day =
    let azimuth = sunsetAzimuth day
        unit = pi / 12
        hours = (azimuth / unit) - 1             -- since utc is 1 hour behind cet
    in  UTCTime day (realToFrac hours * realToFrac hourDuration)

sunriseTime :: Day -> UTCTime
sunriseTime day =
    let sunset = sunsetTime day
        noon = UTCTime day (12 * realToFrac hourDuration)
        timeFromNoon = diffUTCTime sunset noon
    in  addUTCTime (-timeFromNoon) noon


-- + -------------------------------------------------------------------- + --

sunAzimuth :: UTCTime -> Float
sunAzimuth = angleAtTime sunGear

moonAzimuth :: UTCTime -> Float
moonAzimuth = angleAtTime moonGear

-- + -------------------------------------------------------------------- + --

-- Creates circle from rotation (in radians) of the Zodiac gear 
zodiacCircle :: UTCTime -> Circle
zodiacCircle utc = Circle center zodiacRadius
    where gearAngle = angleAtTime zodiacGear utc
          center = scaleVector zodiacDistanceFromOrigin (unitVector gearAngle)

-- Finds Sun's position on the dial, based on zodiac circle, and the rotation of the sun gear
sunPosition :: UTCTime -> Vec2
sunPosition utc =
    let sunHandle = Ray originPoint (unitVector $ sunAzimuth utc)
        zodiac = zodiacCircle utc
    in  fst $ fromJust $ rayCircleIntersection sunHandle zodiac

-- Finds the azimuth at which Sun rises, from sun's position on the dial
sunriseAzimuth :: Day -> Float
sunriseAzimuth day =
    let sunPos = sunPosition (UTCTime day 0)
        dayCircle = Circle originPoint (magnitude sunPos)
        (Vec2 x1 y1, Vec2 x2 y2) = fromJust $ circlesIntersection dayCircle horizonLine
    in  if x1 < x2 then azimuth (Vec2 x1 y1) - pi           -- -pi, since azimuth is 0, when the sun points upwards
        else azimuth (Vec2 x2 y2) - pi

-- Finds the altitude at which Sun sets, from sun's position on the dial
sunsetAzimuth :: Day -> Float
sunsetAzimuth utc = - (sunriseAzimuth utc)

-- [[ --------------------------- Geometry ----------------------------- ]] --
-- [[ ------------------------------------------------------------------ ]] --

-- 2D Vector (x coordinate, y coordinate)
data Vec2 = Vec2 Float Float deriving (Eq)

addVectors :: Vec2 -> Vec2 -> Vec2
addVectors (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

negVector :: Vec2 -> Vec2
negVector (Vec2 x y) = Vec2 (-x) (-y)

subVectors :: Vec2 -> Vec2 -> Vec2
subVectors v1 v2 = addVectors v1 (negVector v2)

scaleVector :: Float -> Vec2 -> Vec2
scaleVector t (Vec2 x y) = Vec2 (t * x) (t * y)

dot :: Vec2 -> Vec2 -> Float
dot (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2

magnitude :: Vec2 -> Float
magnitude (Vec2 x y) = pythagorean x y

-- Returns angle in radians, as if the point was in polar coordinates
azimuth :: Vec2 -> Float
azimuth (Vec2 x y) = atan2 y x

normalized :: Vec2 -> Vec2
normalized v = scaleVector (1 / magnitude v) v

-- Returns vector of magnitude 1, with azimuth given in radians
unitVector :: Float -> Vec2
unitVector angle = Vec2 (cos angle) (sin angle)

originPoint :: Vec2
originPoint = Vec2 0 0

instance Show Vec2 where
    show (Vec2 x y) = "[" ++ show x ++ ", " ++ show y ++ "]"

-- + -------------------------------------------------------------------- + --

-- Ray (ray origin, direction unit Vector)
data Ray = Ray Vec2 Vec2

-- Return point lying on ray at specified parameter t
rayPointAt :: Ray -> Float -> Vec2
rayPointAt (Ray origin direction) t = addVectors origin (scaleVector t direction)


-- + -------------------------------------------------------------------- + --

-- Circle (center, radius)
data Circle = Circle Vec2 Float
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
    | Vec2 x1 x2 == Vec2 x1 x2 = Nothing        -- Circles have the same center
    | x1 == x2 =
        let c = (- x1^2 + x2^2 - y1^2 + y2^2 + r1^2 - r2^2) / (2 * (y2 - y1)) - y1
            d = (x2 - x1) / (y2 - y1)

            xRoots = quadraticFormula (d^2 + 1) (-2 * c * d - 2 * x1) (-r1^2 + c^2 + x1^2)
            yFromX x = (c + y1) - d * x

        in  if isNothing xRoots then Nothing
            else Just $ mapTuple (\ x -> Vec2 (yFromX x) x) (fromJust xRoots)

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
quadraticFormula :: Float -> Float -> Float -> Maybe (Float, Float)
quadraticFormula a b c =
    let d = b^2 - 4 * a * c
    in  if d < 0 then Nothing
        else if a == 0 then Just ((-c) / b, (-c) /b)
        else Just ((-b + sqrt d) / (2 * a),
                   (-b - sqrt d) / (2 * a))

-- Computes the length of hypotenuse from the Pythagorean theorem
pythagorean :: Float -> Float -> Float
pythagorean a b = sqrt (a^2 + b^2)


-- + -------------------------------------------------------------------- + --

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)