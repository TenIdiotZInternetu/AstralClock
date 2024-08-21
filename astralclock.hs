import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Fixed (Pico)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

type ReferenceTime = UTCTime

minuteDuration = 60
hourDuration = 60 * minuteDuration
dayDuration = 24 * hourDuration

class Clock c where
    fromUtc :: c -> UTCTime -> ClockWithValue c
    timeAtZero :: c -> ReferenceTime -> UTCTime
    unitInterval :: c -> ReferenceTime -> NominalDiffTime
    revolutionInterval :: c -> ReferenceTime -> NominalDiffTime


toUtc :: Clock c => ClockWithValue c -> ReferenceTime -> UTCTime
toUtc (clock, value) refTime =
    let unit = unitInterval clock refTime
        zeroTime = timeAtZero clock refTime
        timeOnInterval = toInteger (unit * value)
    in  addUTCTime (timeOnInterval * unit) zeroTime


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
             else addUTCTime (revolutionInterval clock curUtcTime) curUtcTime

