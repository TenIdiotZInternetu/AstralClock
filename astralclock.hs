import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Fixed (Pico)

class Clock c where
    fromUtc :: UTCTime -> c
    toInterval :: c -> NominalDiffTime


-- Returns the state off clock at current time
now :: Clock c => IO c
now = do
    fromUtc <$> getCurrentTime



-- Returns the next time the clock will be in the given state
-- nextTime :: Clock -> Clock
-- nextTime 

-- Returns the state of clock after the given time passes
inTime :: Clock c => c -> IO c
inTime clock = do
    currentTime <- getCurrentTime
    let resultTime = addUTCTime (toInterval clock) currentTime
    return $ fromUtc resultTime


instance Clock UTCTime where
    fromUtc time = time
    toInterval (UTCTime date seconds) =
        let days = fromIntegral $ snd $ toOrdinalDate date :: NominalDiffTime
        in days * nominalDay + realToFrac seconds

