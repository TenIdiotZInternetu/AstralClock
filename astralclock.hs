import Data.Time

class Clock t where
    toUtc :: t -> Maybe UTCTime
    fromUtc :: UTCTime -> t


-- Returns the state off clock at current time
now :: Clock

-- Returns the next time the clock will be in the given state
nextTime :: Clock -> Clock

-- Returns the state of clock after the given time passes
inTime :: Clock -> Clock