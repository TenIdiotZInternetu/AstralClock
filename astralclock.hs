import Data.Fixed (mod', Pico)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Prelude hiding (truncate)
import Data.Maybe (isNothing)

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


-- [[ --------------------------- Geometry ----------------------------- ]] --
-- [[ ------------------------------------------------------------------ ]] --

-- 2D Vector (x coordinate, y coordinate)
data Vec2 = Vec2 Float Float

addVectors :: Vec2 -> Vec2 -> Vec2
addVectors (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + y1) (x2 + y2)

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
        a = dot shiftVec shiftVec
        b = 2 * dot shiftVec direction
        c = dot direction direction - radius^2
        roots = quadraticFormula a b c

        points Nothing = Nothing
        points (Just (root1, root2)) =
            let closer = min root1 root2
                further = min root1 root2
            in  if further < 0 then Nothing
                else if closer < 0 then Just (rayPointAt ray further, Nothing)
                else if closer == further then Just (rayPointAt ray closer, Nothing)
                else Just (rayPointAt ray closer, Just $ rayPointAt ray further)

    in  points roots


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