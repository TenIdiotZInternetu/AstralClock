import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Fixed (Pico)
import GHC.IO.Encoding (setLocaleEncoding, utf8)

type ReferenceTime = UTCTime

class Clock c where
    fromUtc :: c -> UTCTime -> c val
    toUtc :: c val -> ReferenceTime -> UTCTime
    unitInterval :: c -> ReferenceTime -> NominalDiffTime
    revolutionInterval :: c -> ReferenceTime -> NominalDiffTime


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

