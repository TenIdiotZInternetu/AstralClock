import Data.Fixed (mod', Pico)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Prelude hiding (truncate)
import System.Win32 (COORD(yPos))

romanNumerals = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"]

-- Cartesian (x, y) | Polar (radius, angle in radians)
data Point = Cartesian Float Float | Polar Float Float

-- Circle (center, radius)
data Circle = Circle Point Float




-------------------------------- Helping functions --------------------------------
-----------------------------------------------------------------------------------
