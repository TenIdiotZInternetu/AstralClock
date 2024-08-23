import Data.Fixed (mod', Pico)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Prelude hiding (truncate)

romanNumerals = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"]

----------------------------- Time Constants ------------------------------
---------------------------------------------------------------------------

minuteDuration :: NominalDiffTime
minuteDuration = 60

hourDuration :: NominalDiffTime
hourDuration = 60 * minuteDuration

dayDuration :: NominalDiffTime
dayDuration = 24 * minuteDuration


-------------------------------- Geometry ---------------------------------
---------------------------------------------------------------------------

-- Cartesian (x, y) | Polar (radius, angle in radians)
data Point = Cartesian Float Float | Polar Float Float

-- Circle (center, radius)
data Circle = Circle Point Float

-- Ray (ray origin, direction unit Vector)
data Ray = Ray Point Vec2

origin :: Point
origin = Cartesian 0 0

origin :: Vec2
origin = Vec2 0 0

toVec2 :: Point -> Vec2
toVec2 point = Vec2 x y
    where (Cartesian x y) = toCartesian point


toCartesian :: Point -> Point
toCartesian (Polar radius angle) = Cartesian (radius * cos angle) (radius * sin angle)
toCartesian (Cartesian x y) = Cartesian x y

toPolar :: Point -> Point
toPolar (Cartesian x y) = Polar (pythagorean x y) (atan2 y x)
toPolar (Polar radius angle) = Polar radius angle


-- Computes the length of hypotenuse from the Pythagorean theorem
pythagorean :: Float -> Float -> Float
pythagorean a b = sqrt (a^2 + b^2)


-- Returns intersections of ray and circle if they exist, return Nothing if not
-- First point is closer to the ray origin, second is further.
rayCircleIntersection :: Ray -> Circle -> Maybe (Point, Point)
rayCircleIntersection (Ray orig direction) (Circle center radius) =
    let originVec = toVec2 orig
        a = dot originVec originVec
        b = 2 * dot originVec direction
        c = dot direction direction


-- Solves x for ax^2 + bx + c = 0
-- Returnes both solutions if they exist, returns Nothing if none exist
quadraticFormula :: Float -> Float -> Float -> Maybe (Float, Float)
quadraticFormula a b c =
    let d = b^2 - 4 * a * c
    in  ((-b + sqrt d) / 2 * a,
         (-b - sqrt d) / 2 * a)

-------------------------------- Astrolabe --------------------------------
---------------------------------------------------------------------------

cancerTropic :: Circle
cancerTropic = Circle origin 1

equator :: Circle
equator = Circle origin 0.6556

ariesTropic :: Circle
ariesTropic = Circle origin 4.298


horizonLine :: Circle
horizonLine = Circle (Cartesian 0.0 (-0.7838)) 1.0219

twilightLine :: Circle
twilightLine = Circle (Cartesian 0.0 (-0.5385)) 0.7083


zodiacRadius :: Float
zodiacRadius = 0.7149

zodiacDistanceFromOrigin :: Float
zodiacDistanceFromOrigin = 0.2851


----------------------------------- Gears -----------------------------------
-----------------------------------------------------------------------------

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