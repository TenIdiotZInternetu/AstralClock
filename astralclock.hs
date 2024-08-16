import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Fixed (Pico)

class Clock c where
    fromUtc :: UTCTime -> IO c
    toInterval :: c -> NominalDiffTime


-- Returns the state off clock at current time
now :: Clock c => IO c
now = do
    currentTime <- getCurrentTime
    fromUtc currentTime

-- Returns the next time the clock will be in the given state
-- nextTime :: Clock -> Clock
-- nextTime 

-- Returns the state of clock after the given time passes
inTime :: Clock c => c -> IO c
inTime clock = do
    currentTime <- getCurrentTime
    fromUtc $ addUTCTime (toInterval clock) currentTime


instance Clock UTCTime where
    fromUtc = return
    toInterval (UTCTime date seconds) = 
        let days = fromIntegral $ snd $ toOrdinalDate date :: NominalDiffTime
        in days * nominalDay + realToFrac seconds