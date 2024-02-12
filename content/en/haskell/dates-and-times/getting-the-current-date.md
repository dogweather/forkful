---
title:                "Getting the current date"
aliases: - /en/haskell/getting-the-current-date.md
date:                  2024-02-03T19:02:31.376532-07:00
model:                 gpt-4-0125-preview
simple_title:         "Getting the current date"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Retrieving the current date in Haskell involves obtaining the system's current time and transforming it into a readable date format. Programmers do this to perform operations based on the date, such as logging, scheduling tasks, or timestamping events in applications.

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
