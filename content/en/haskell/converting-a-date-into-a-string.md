---
title:                "Converting a date into a string"
html_title:           "Haskell recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often face the task of converting data from one type to another. One common conversion is from a date data type to a string data type. This may be necessary for displaying dates in a specific format or for storing dates in a database that only accepts strings. In this article, we will explore how to convert a date into a string in Haskell, a powerful functional programming language.

## How To

First, we need to import the `Data.Time.Format` module, which provides functions for formatting dates. We will also import the `Data.Time` module, which provides various data types and functions for working with dates.

```Haskell
import Data.Time.Format
import Data.Time
```

Next, we need to define a `formatTime` function, which takes in a `String` representing the desired format and a `UTCTime` data type representing the date. The `UTCTime` type is used to represent a specific time on a specific day, in a specific time zone.

```Haskell
formatTime :: String -> UTCTime -> String
```

Now, we can use the `formatTime` function to convert a date into a string. Let's say we have a `UTCTime` value representing the current date and time, using the `getCurrentTime` function from the `Data.Time.Clock` module.

```Haskell
dateString :: String
dateString = formatTime "%Y-%m-%d" getCurrentTime

-- Output: "2021-01-01"
```

The `%Y`, `%m`, and `%d` are format codes that represent the year, month, and day respectively. You can use other format codes for different date formats, such as `%H` for hour, `%M` for minute, and `%S` for second.

## Deep Dive

In the above example, we used the `UTCTime` type, but Haskell also provides other types for representing dates, such as `LocalTime`, `ZonedTime`, and `Day`. If you need to work with different time zones, you can use the `zonedTime` function from the `Data.Time.LocalTime.TimeZone` module to convert a `UTCTime` to a `ZonedTime`.

Another useful function for converting dates to strings is the `formatTimeLocale` function, which allows you to specify a specific locale for formatting. This is especially helpful when working with non-English languages.

You can also customize the date format by using the `Data.Time.Format.TimeLocale` module to define your own time locale with your desired format codes.

## See Also

- [Haskell Data.Time module](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskell Data.Time.Format module](https://hackage.haskell.org/package/time/docs/Data-Time-Format.html)
- [Haskell Data.Time.LocalTime.TimeZone module](https://hackage.haskell.org/package/time/docs/Data-Time-LocalTime-TimeZone.html)
- [Formatting Dates and Times in Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20date%20formatting)