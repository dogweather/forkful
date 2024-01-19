---
title:                "Calculating a date in the future or past"
html_title:           "Haskell recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calculating a date in the future or past means determining what the date will be after or before a set period. Programmers use such computations for tasks like creating event reminders or scheduling automated tasks.

## How to:

```Haskell
import Data.Time.Clock (UTCTime, addUTCTime, secondsToDiffTime)
import Data.Time.Calendar (Day, addDays)

-- Add days to a date
addDaysTo :: Day -> Integer -> Day
addDaysTo date days = addDays days date

-- Usage Example:
-- Suppose today's date (2022-03-13) and we want to find the date after 30 days
let futureDate = addDaysTo (fromGregorian 2022 03 13) 30

-- Add seconds to a UTC time
addSecondsTo :: UTCTime -> Int -> UTCTime
addSecondsTo time seconds = addUTCTime (secondsToDiffTime $ fromIntegral seconds) time

-- Usage Example:
-- Suppose now is (2022-03-13 12:00:00 UTC) and we want to find the time 3600 seconds later
let futureTime = addSecondsTo  (UTCTime (fromGregorian 2022 03 13) (timeOfDayToTime $ TimeOfDay 12 00 00)) 3600
```

## Deep Dive

Ever since computers started handling dates, programmers have had to work out future or past dates. While you might want to calculate this "manually" by adding or subtracting days, months, or years, this way is error-prone considering the complexity of our Gregorian calendar with leap years and varying days per month. 

Haskell's `Data.Time` library conveniently does all this for us. `addDays` and `addUTCTime` from this library allow us to adjust dates and UTC times relatively. 

Alternatives include using libraries like `thyme` or `chronos`, but `Data.Time` is the most popular for its simplicity and ease of use. It uses UTCTime for time manipulations, which avoids time zone complications intrinsic with local times.

## See Also

1. [Data.Time Library Documentation](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
2. [A comprehensive guide on date and time in Haskell](https://two-wrongs.com/haskell-time-library-tutorial)
3. [StackOverflow Discussion on Date Manipulation in Haskell](https://stackoverflow.com/questions/38312482/haskell-transform-utctime-to-day-and-add-days)