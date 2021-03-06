module Data.Logging where

import Data.Time.Clock
import Data.List

data LogLevel = Debug | Info | Warning | Error | Fatal
              deriving (Show, Read, Eq, Ord, Enum, Bounded)

data EventSource = Internal { iesComponent   :: String
                            , iesCallID      :: String }
                 | External { eesURI         :: String
                            , eesDescription :: String }
                 | Combined [EventSource]
                 | Unknown
                 deriving (Show, Read, Eq)

data LogMessage = LogMessage
                { lmSource     :: EventSource
                , lmMessage    :: String
                , lmTimestamp  :: UTCTime
                , lmHiddenFlag :: Bool
                , lmLogLevel   :: LogLevel
                } deriving (Show, Read, Eq)

data EventSourceMatcher = Exact EventSource
                        | With EventSource
                        | AnyInternal
                        | AnyExternal
                        | Any
                        | MatchAny [EventSourceMatcher]
                        | MatchAll [EventSourceMatcher]
                        deriving (Show, Read, Eq)

-- | Change log level operator
-- TODO: implement operator which changes LogLevel of LogMessage
($=) :: LogMessage -> LogLevel -> LogMessage
($=) = undefined


-- | EventSource "combinator"
-- TODO: implement operator which combines two EventSources (just 1 level for Combined, see tests)
(@@) :: EventSource -> EventSource -> EventSource
(@@) = undefined

-- | Matching EventSource with EventSourceMatcher operator
-- TODO: implement matching
infixr 6 ~~
(~~) :: EventSourceMatcher -> EventSource -> Bool
(~~) = undefined

-- | Specialized log list filter
-- TODO: implement filter function for logs with matchers, log level and hidden flag
logFilter :: EventSourceMatcher -> LogLevel -> Bool -> [LogMessage] -> [LogMessage]
logFilter  = undefined
