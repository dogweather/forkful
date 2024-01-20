---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in Haskell means fetching the present system date. Programmers use this feature in instances where date stamping or time-related calculations are necessary in the code.

## How to:

First, import the Data.Time library:

```Haskell
import Data.Time
```

Then, get the current date using the `getCurrentTime` function:

```Haskell
main :: IO ()
main = do
    current_date <- getCurrentTime
    print current_date
```

This will print the current date and time including precise nanoseconds: 

```Haskell
2022-03-06 14:35:12.1975212 UTC
```

If you want just the date and not the time, get the Day value, which is one part of the UTCTime value:

```Haskell
import Data.Time.Clock
import Data.Time.Calendar

main :: IO ()
main = do
    now <- getCurrentTime
    let currentDay = utctDay now
    print currentDay
```

This will output:

```Haskell
2022-03-06
```

## Deep Dive

Getting the current date has long been a standard feature in many programming languages, with Haskell no exception. UTCTime is the typical method of dealing with date and time in Haskell and it's represented as the number of seconds since the UNIX epoch - 00:00:00 UTC, Thursday, 1 January 1970.

There are alternative ways to get the current date and time in Haskell, such as using the `getZonedTime` function to get the current date and time in the local time zone.

The `getCurrentTime` function comes from the base library, and it uses a straightforward system call to fetch the time. It's a reliable lightweight function with a minimal footprint in your code.

## See Also

For more details on Haskell's date and time functions, check the following links:

- [Haskell Data.Time library documentation](http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)