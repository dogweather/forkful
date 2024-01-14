---
title:    "Haskell recipe: Comparing two dates"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why Comparing Dates in Haskell is Useful

When working with dates and time in Haskell, it can be useful to compare two dates in order to determine which one came first, or to check if they are the same. This can be especially helpful when working with data that includes dates, as well as when dealing with specific events or schedule planning.

## How To Compare Dates in Haskell

Comparing two dates in Haskell is straightforward, due to the built-in `Ord` type class which allows for comparisons between values. We can use this type class to compare two dates by first converting them to the `UTCTime` type, which represents a specific moment in time.

Here is a simple example of comparing two dates using the `compare` function:

```Haskell
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate

-- Create two dates
let date1 = fromGregorian 2020 01 10
let date2 = fromGregorian 2020 01 15

-- Convert dates to UTCTime type
let utcDate1 = UTCTime date1 (secondsToDiffTime 0)
let utcDate2 = UTCTime date2 (secondsToDiffTime 0)

-- Compare the two dates
print $ compare utcDate1 utcDate2
```

Running this code will output: `LT` indicating that `date1` comes before `date2`. If the dates were reversed, the output would be `GT` indicating that `date2` comes before `date1`. If the dates were the same, the output would be `EQ`.

## Deep Dive into Comparing Dates

When comparing dates in Haskell, it's important to note that the `Ord` instance for `UTCTime` does not take into account time zones. This means that two `UTCTime` values with different time zones may be considered equal when comparing them using `compare`. 

To take time zones into account, we can use the `CalendarDiffDays` module from the `Data.Time.Calendar.OrdinalDate` package. This module provides functions for calculating the difference between two dates in days, taking the time zone into consideration.

Here is an example of calculating the difference in days between two dates, using the `diffDays` function:

```Haskell
import Data.Time.Calendar.OrdinalDate

-- Create two dates with different time zones
let date1 = fromGregorian 2020 01 10
let date2 = fromGregorian 2020 01 15

-- Calculate the difference in days
print $ diffDays date1 date2
```

Running this code will output: `5` indicating that there are 5 days between `date1` and `date2`.

## See Also
- [Haskell documentation on comparing dates](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Calendar.html#t:Day)
- [Tutorial on working with dates and time in Haskell](http://learnyouahaskell.com/input-and-output#files-and-streams)
- [Official Haskell website](https://www.haskell.org/)