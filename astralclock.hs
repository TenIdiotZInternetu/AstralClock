import Data.Fixed (mod', Pico)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Prelude hiding (truncate)
import Data.Maybe (isNothing, fromJust)

romanNumerals = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"]

-- [[ ------------------------ Time Constants -------------------------- ]] --
-- [[ ------------------------------------------------------------------ ]] --

minuteDuration :: NominalDiffTime
minuteDuration = 60

hourDuration :: NominalDiffTime
hourDuration = 60 * minuteDuration

dayDuration :: NominalDiffTime
dayDuration = 24 * minuteDuration


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

-- Gear (revolution duration)
newtype Gear = Gear NominalDiffTime

angularSpeed :: Gear -> Float
angularSpeed (Gear revolution) = realToFrac (2 * pi) / realToFrac revolution

-- Returns angular distance (radians) the gear rotated in given time
rotationAfterTime :: Gear -> NominalDiffTime -> Float
rotationAfterTime gear time = realToFrac time * angularSpeed gear

-- + -------------------------------------------------------------------- + --

sunGear :: Gear
sunGear = Gear dayDuration

zodiacGear :: Gear
zodiacGear = Gear $ 365 / 366 * dayDuration

moonGear :: Gear
moonGear = Gear $ 378.8 / 365 * dayDuration

moonPhaseGear :: Gear
moonPhaseGear = Gear $ 29.5305 * dayDuration

calendarGear :: Gear
calendarGear = Gear $ 365 * dayDuration


-- [[ --------------------------- Clocks ------------------------------- ]] --
-- [[ ------------------------------------------------------------------ ]] --

class Clock c where
    fromUtc :: c -> UTCTime

class ClockValue val where
    clock :: Clock c => val -> c


data CETClock = CETClock
-- instance Clock CETClock where
--     clockGear _ = sunGear
--     value _ angle = 

newtype CETClockValue = CETClockVal Float

-- + -------------------------------------------------------------------- + --

zodiacCircle :: Float -> Circle
zodiacCircle gearAngle = Circle center zodiacRadius
    where center = scaleVector zodiacDistanceFromOrigin (unitVector gearAngle)

sunPosition :: Circle -> Float -> Vec2
sunPosition zodiac sunAngle = 
    let sunHandle = Ray originPoint (unitVector sunAngle) 
    in  fst $ fromJust $ rayCircleIntersection sunHandle zodiac

sunriseAltitude :: Vec2 -> Float
sunriseAltitude sunPosition = 
    let dayCircle = Circle originPoint (magnitude sunPosition)
        (Vec2 x1 y1, Vec2 x2 y2) = fromJust $ circlesIntersection dayCircle horizonLine
    in  if x1 < x2 then azimuth (Vec2 x1 y1) - pi           -- -pi, since altitude is 0, when the sun points upwards
        else azimuth (Vec2 x2 y2) - pi

sunsetAltitude :: Vec2 -> Float
sunsetAltitude sunPosition = - (sunriseAltitude sunPosition)

-- [[ --------------------------- Geometry ----------------------------- ]] --
-- [[ ------------------------------------------------------------------ ]] --

-- 2D Vector (x coordinate, y coordinate)
data Vec2 = Vec2 Float Float

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

circlesIntersection :: Circle -> Circle -> Maybe (Vec2, Vec2)
circlesIntersection (Circle (Vec2 x1 y1) r1) (Circle (Vec2 x2 y2) r2) =
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