---
title:                "Calculating a date in the future or past"
aliases:
- en/haskell/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:13.320023-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculating a date in the future or past"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a future or past date means finding a date before or after a given number of days, months, or years from a specific starting point. Programmers do this for things like expiration dates, scheduling, or determining the time elapsed between events.

## How to:

Haskell uses libraries like `time` to deal with dates. Here's how to add days or months to a date, or subtract them to find a past date.

```Haskell
import Data.Time

-- Add days to the current date
addDaysToCurrent :: Integer -> IO Day
addDaysToCurrent n = do
  today <- getCurrentTime
  timezone <- getCurrentTimeZone
  let localToday = utcToLocalTime timezone today
  return $ addDays n (localDay localToday)

-- Usage: addDaysToCurrent 10 to add 10 days to the current date

-- Calculate a future or past date by adding or subtracting days
calculateDate :: Day -> Integer -> Day
calculateDate start n = addDays n start

-- Usage example:
-- let futureDate = calculateDate (fromGregorian 2023 1 1) 90

-- To handle months and years, we use `addGregorianMonthsClip` and `addGregorianYearsClip`
calculateDateMonths :: Day -> Integer -> Day
calculateDateMonths start n = addGregorianMonthsClip n start

-- Usage:
-- let futureMonth = calculateDateMonths (fromGregorian 2023 1 1) 2

-- Output a date in the format YYYY-MM-DD
printFormattedDate :: Day -> IO ()
printFormattedDate date = putStrLn $ formatTime defaultTimeLocale "%F" date

-- Usage:
-- printFormattedDate futureDate
```

## Deep Dive

In Haskell, we often reach for the `time` library for date calculations. This library provides types and functions for DateTime arithmetic, parsing, and formatting. Historically, people would manually adjust dates, but libraries like `time` handle the quirks of calendars (like leap years).

Alternatives to `time` include `Data.Time.Calendar.OrdinalDate` and `Data.Time.Clock.POSIX` for different needs, like working with week numbers or timestamps.

Implementation wise, calculating dates is surprisingly complex. Even with `time`, functions like `addGregorianMonthsClip` ensure the resulting date is valid. For example, adding one month to January 31st will "clip" to the last day of February (either the 28th or 29th), not March 3rd.

## See Also

- Haskell `time` library: http://hackage.haskell.org/package/time
- Date and Time guide from The Haskell School: https://school.haskellforall.com/#date-and-time
- ZonedTime and UTC explanation: https://www.47deg.com/blog/dealing-with-time-in-haskell/
