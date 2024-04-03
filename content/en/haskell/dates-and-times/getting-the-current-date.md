---
date: 2024-02-03 19:02:31.376532-07:00
description: "How to: Haskell's standard library, `base`, provides the `Data.Time`\
  \ module which offers functionality to work with dates and times. Here's how to\
  \ use it\u2026"
lastmod: '2024-03-13T22:45:00.138036-06:00'
model: gpt-4-0125-preview
summary: Haskell's standard library, `base`, provides the `Data.Time` module which
  offers functionality to work with dates and times.
title: Getting the current date
weight: 29
---

## How to:
Haskell's standard library, `base`, provides the `Data.Time` module which offers functionality to work with dates and times. Here's how to use it to get the current date:

```haskell
import Data.Time (getCurrentTime, utctDay)

main :: IO ()
main = do
    now <- getCurrentTime
    let today = utctDay now
    print today
```

Sample output:
```
2023-04-12
```

For more flexibility, such as formatting the date or working with different time zones, the `time` library is invaluable. Here’s how you might format the current date:

```haskell
import Data.Time

main :: IO ()
main = do
    now <- getCurrentTime
    timezone <- getCurrentTimeZone
    let zoneNow = utcToLocalTime timezone now
    putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d" zoneNow
```

This prints the current date in the `YYYY-MM-DD` format, adjusted to the local time zone.

Additionally, for third-party library support, `time` is highly recommended and often used within the Haskell community for its extensive date and time manipulation capabilities. The examples above utilize this library.

If you need more comprehensive date manipulation, including parsing from strings or arithmetic operations with dates and times, exploring additional functions within `Data.Time` will be beneficial.
