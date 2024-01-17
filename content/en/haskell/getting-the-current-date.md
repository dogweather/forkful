---
title:                "Getting the current date"
html_title:           "Haskell recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in Haskell simply means retrieving the current date and time from the computer's internal clock. This is a common task for programmers as it allows for timestamping events or data and keeping track of time-sensitive tasks.

## How to:

To get the current date and time in Haskell, we can use the `getCurrentTime` function from the `Data.Time.Clock` module. This function returns a `UTCTime` value representing the current time in the UTC time zone. Here's an example of how to use it:

```Haskell
import Data.Time.Clock

main = do
    now <- getCurrentTime
    putStrLn $ show now
```

The output will look something like this: `2021-04-25 12:00:00.000000000 UTC`

If we want to work with a specific time zone, we can use the `getCurrentTimeZone` function to get a `TimeZone` value and then use the `utcToLocalTime` function to convert the `UTCTime` value to the desired time zone. Here's an example:

```Haskell
import Data.Time.Clock
import Data.Time.LocalTime

main = do
    now <- getCurrentTime
    timeZone <- getCurrentTimeZone
    let localTime = utcToLocalTime timeZone now
    putStrLn $ show localTime
```

The output will now be in the local time zone of the computer.

## Deep Dive:

In the past, getting the current date and time in Haskell was more complicated and required the use of libraries like `Time` or `Clock`. However, with the release of Haskell 2010, the `Data.Time` library was included in the standard library, making it easier to work with dates and times.

There are a few alternatives to using the `Data.Time` library, such as the `System.Time` module, which is based on the old `Time` library. However, it is recommended to use the `Data.Time` library as it is more reliable and feature-rich.

The `getCurrentTime` and `getCurrentTimeZone` functions are wrappers around low-level system calls, making them very efficient. However, they can only handle time down to the microsecond level, so if you need more precision, you will have to use a third-party library or implement your own solution.

## See Also:

- [Data.Time.Clock documentation](https://hackage.haskell.org/package/time/docs/Data-Time-Clock.html)
- [Time and Date in Haskell](https://wiki.haskell.org/Time_and_Date)
- [HTime library for high-precision time handling](https://hackage.haskell.org/package/HTime)