---
title:                "Comparing two dates"
html_title:           "Haskell recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
If you've ever needed to compare two dates in your code, whether to determine which one is earlier or to check for a certain time interval, then reading this article might just save you some time and headaches. Comparing dates may seem like a simple task, but it can become tricky and error-prone due to differences in date formats and time zones. With Haskell, we have the tools to easily and accurately compare dates, making our code more efficient and reliable.

## How To
To compare two dates in Haskell, we first need to import the Data.Time library, which provides functions and types for working with dates and times. We will also use the Data.Time.Clock module, specifically the UTCTime type, which represents a Universal Coordinated Time (UTC) in Haskell. Let's see how we can compare two dates using some examples.

### Example 1: Checking if date is before another date
```Haskell
import Data.Time

-- Function to compare two dates
compareDates :: UTCTime -> UTCTime -> Ordering
compareDates date1 date2 = compare date1 date2

-- Sample dates
today :: IO UTCTime
today = getCurrentTime
tomorrow = addUTCTime (86400 :: NominalDiffTime) today

-- Output
>>> compareDates <$> today <*> tomorrow
LT
```
In this example, we first import the Data.Time library and define a function called `compareDates` that takes in two UTC dates and uses the built-in `compare` function to return an `Ordering` value, which can be either `LT` (Less Than), `GT` (Greater Than), or `EQ` (Equal). We then create two sample dates using the `getCurrentTime` and `addUTCTime` functions, and finally, we use the `compareDates` function to compare them, outputting `LT` since `tomorrow` comes after `today`.

### Example 2: Checking if date falls within a certain time interval
```Haskell
import Data.Time

-- Function to check if date is within a time interval
withinInterval :: UTCTime -> UTCTime -> UTCTime -> Bool
withinInterval date start end = date <= end && date >= start

-- Sample dates
today :: IO UTCTime
today = getCurrentTime
startOfNextMonth = addUTCTime (2592000 :: NominalDiffTime) today
endOfNextMonth = addUTCTime (5184000 :: NominalDiffTime) today

-- Output
>>> withinInterval <$> startOfNextMonth <*> today <*> endOfNextMonth
True
```
In this example, we define a function called `withinInterval` that takes in a date, a start date, and an end date and checks if the date falls within the specified time interval. We use the `getCurrentTime` and `addUTCTime` functions to create sample dates, and then use the `withinInterval` function to check if `today` falls within the interval between the `startOfNextMonth` and `endOfNextMonth`, which returns `True` since `today` is indeed within that interval.

## Deep Dive
The `compare` function used in `compareDates` is a part of the Ord typeclass, which is used for types that have an ordering. This means that we can use it not only for dates but also for other types such as numbers or strings. Similarly, the `<=` and `>=` operators used in `withinInterval` are also part of the Ord typeclass, allowing us to compare dates just like we would compare other types.

When comparing dates in Haskell, it's important to keep in mind that dates are represented as UTC values, so if you're working with dates in a different time zone, you may need to adjust them using functions such as `ZonedTime` in the Data.Time.LocalTime module.

## See Also
- [Data.Time documentation](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Comparison Operators in Haskell](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Ord)
- [Working with time zones in Haskell](https://hackage.haskell.org/package/time/docs/Data-Time-LocalTime.html)