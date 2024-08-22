import Data.Fixed (mod', Pico)
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Prelude hiding (truncate)

romanNumerals = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X", "XI", "XII"]

-- Specifies which  time interval should be considered for further calculations
type ReferenceTime = UTCTime

-- A clock type and clock value pair
-- Clock c =>
data ClockWithValue c = Clock c NominalDiffTime

minuteDuration :: NominalDiffTime
minuteDuration = 60

hourDuration :: NominalDiffTime
hourDuration = 60 * minuteDuration

dayDuration :: NominalDiffTime
dayDuration = 24 * hourDuration


class Clock c where
    -- Returns clock of specified type with value corresponding to the UTC
    fromUtc :: c -> UTCTime -> ClockWithValue c

    -- Returns duration of a single unit of the specified clock,
    -- on interval specified by the ReferenceTime
    unitDuration :: c -> ReferenceTime -> NominalDiffTime

    -- Return total duration of a single interval specified by the ReferenceTime, on the specified clock
    intervalDuration :: c -> ReferenceTime -> NominalDiffTime


-- Returns UTC time at which the specified clock has the value 0,
-- on interval specified by the ReferenceTime
timeAtZero :: Clock c => c -> ReferenceTime -> UTCTime
timeAtZero clock refTime =
    let Clock _ refClockValue = fromUtc clock refTime
        unit = unitDuration clock refTime
    in addUTCTime (refClockValue * (-unit)) refTime


-- Returns UTC time that corresponds to the value shown on the specified clock,
-- on interval specified by the ReferenceTime
toUtc :: Clock c => ClockWithValue c -> ReferenceTime -> UTCTime
toUtc (Clock clock value) refTime =
    let unit = unitDuration clock refTime
        zeroTime = timeAtZero clock refTime
    in  addUTCTime (value * unit) zeroTime


-- Returns the clock and its value at the current time
now :: Clock c => c -> IO (ClockWithValue c)
now clock = do
    fromUtc clock <$> getCurrentTime


-- Returns the clock and its value at NominalDiffTime in the future
inTime :: Clock c => c -> NominalDiffTime -> IO (ClockWithValue c)
inTime clock time = do
    newTime <- addUTCTime time <$> getCurrentTime
    return $ fromUtc clock newTime


-- Returns nearest UTC time in the future,
-- at which the specified clock shows the given value
next :: Clock c => ClockWithValue c -> IO UTCTime
next (Clock clock value) = do
    Clock _ curValue <- now clock
    curUtcTime <- getCurrentTime
    let doesFullRotation = value < curValue
    let timeInCurrentRotation = toUtc (Clock clock value) curUtcTime

    return $ if not doesFullRotation then timeInCurrentRotation
             else addUTCTime (intervalDuration clock curUtcTime) curUtcTime

------------------------------ Clock Implementations ------------------------------
-----------------------------------------------------------------------------------

-- Central European Time  -  UTC+1
-- Clock is divided into 12 equal parts, two full rotations add up to one day

-- On Prague Astral Clock its shown by the hand with finger at the tip,
-- pointing to the Roman numerals on the inner dial

-- Valid values are 0-11 mapped to I-XII Roman numerals

data CETClock = CETClock
instance Clock CETClock where
    fromUtc CETClock (UTCTime day secs) =
        Clock CETClock $ (hour + 1) `mod'` 12
            where hour = min 24 (realToFrac secs / realToFrac hourDuration)

    unitDuration CETClock _ = hourDuration

    intervalDuration CETClock _ = 24 * hourDuration


instance Show (ClockWithValue CETClock) where
    show (Clock CETClock value) =
        "CET Clock -> " ++ (romanNumerals !! floor value) ++ "   ." ++ decimals
            where decimals = showDecimals $ realToFrac $ truncate value 3 `mod'` 1


-- Old Czech Time  -  https://cs.wikipedia.org/wiki/Vlašské_hodiny
-- Day starts at sunset, with 1 as its first value, and is divided into 24 equal parts

-- On Prague Astral Clock its shown by the hand with finger at the tip,
-- pointing to the Arabian numerals on the outer dial

-- Valid values are 0-22 mapped to 1-23 Arabian numerals

-------------------------------- Helping functions --------------------------------
-----------------------------------------------------------------------------------

-- Floors the number to n decimal places
truncate :: RealFrac num => num -> Int -> num
truncate number decimals = 
    fromIntegral (floor (number * t)) / t
        where t = 10^decimals

-- Turn only the decimal part of number to string
showDecimals :: (RealFrac num, Show num) => num -> String
showDecimals number =
    let decimalPart = snd $ properFraction number
        dotPosition = if decimalPart < 0 then 3
                      else 2
    in  drop dotPosition $ show decimalPart
