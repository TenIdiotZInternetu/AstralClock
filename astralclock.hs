import Data.Fixed (mod', Pico)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Prelude hiding (truncate)
import System.Win32 (COORD(yPos))

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


origin :: Point
origin = Cartesian 0 0


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