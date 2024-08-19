import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Fixed (Pico)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

type ReferenceTime = UTCTime

minuteDuration = 60
hourDuration = 60 * minuteDuration
dayDuration = 24 * hourDuration

class Clock c where
    fromUtc :: UTCTime -> c val
    timeAtZero :: c -> ReferenceTime -> UTCTime
    unitInterval :: c -> ReferenceTime -> NominalDiffTime
    revolutionInterval :: c -> ReferenceTime -> NominalDiffTime


toUtc :: Clock c => c val -> ReferenceTime -> UTCTime
toUtc clock value refTime =
    let unit = unitInterval clock refTime
        timeAtZero = timeAtZero clock refTime
    in  addUTCTime (value * unit) timeAtZero


-- Returns the state off clock at current time
now :: Clock c => c -> IO (c val)
now clock = do
    fromUtc clock <$> getCurrentTime


inTime :: Clock c => c -> NominalDiffTime -> IO (c val)
inTime clock time = do
    let newTime = addUTCTime time <$> getCurrentTime
    return $ fromUtc clock newTime


next :: Clock c => c val -> IO UTCTime
next clock value = do
    curTime <- getCurrentTime
    let doesFullRotation = value < curTime
    let closestTime = toUtc (clock value) curTime

    return $ if doesFullRotation then closestTime
             else addUTCTime (revolutionInterval clock curTime) curTime

