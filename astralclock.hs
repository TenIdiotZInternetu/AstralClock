import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Fixed (Pico)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Distribution.Utils.Structured (Structure(Nominal))

type ReferenceTime = UTCTime
type ClockWithValue c = (c, NominalDiffTime)

minuteDuration :: NominalDiffTime
minuteDuration = 60

hourDuration :: NominalDiffTime
hourDuration = 60 * minuteDuration

dayDuration :: NominalDiffTime
dayDuration = 24 * hourDuration

class Clock c where
    fromUtc :: c -> UTCTime -> ClockWithValue c
    unitDuration :: c -> ReferenceTime -> NominalDiffTime
    intervalDuration :: c -> ReferenceTime -> NominalDiffTime


timeAtZero :: Clock c => c -> ReferenceTime -> UTCTime
timeAtZero clock refTime =
    let refClockValue = snd $ fromUtc clock refTime
        unit = unitDuration clock refTime
    in addUTCTime (refClockValue * unit * (-1)) refTime

toUtc :: Clock c => ClockWithValue c -> ReferenceTime -> UTCTime
toUtc (clock, value) refTime =
    let unit = unitDuration clock refTime
        zeroTime = timeAtZero clock refTime
    in  addUTCTime (value * unit) zeroTime


-- Returns the state off clock at current time
now :: Clock c => c -> IO (ClockWithValue c)
now clock = do
    fromUtc clock <$> getCurrentTime


inTime :: Clock c => c -> NominalDiffTime -> IO (ClockWithValue c)
inTime clock time = do
    newTime <- addUTCTime time <$> getCurrentTime
    return $ fromUtc clock newTime


next :: Clock c => ClockWithValue c -> IO UTCTime
next (clock, value) = do
    curValue <- snd <$> now clock
    curUtcTime <- getCurrentTime
    let doesFullRotation = value < curValue
    let timeInCurrentRotation = toUtc (clock, value) curUtcTime

    return $ if not doesFullRotation then timeInCurrentRotation
             else addUTCTime (intervalDuration clock curUtcTime) curUtcTime


data CETClock = CETClock
instance Clock CETClock where
    fromUtc CETClock (UTCTime day secs) =
        (CETClock, hour `mod` 12 + 1)
        where hour = secs / hourDuration

    unitDuration CETClock _ = hourDuration

    intervalDuration CETClock _ = 24 * hourDuration