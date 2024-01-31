---
title:                "Getting the current date"
date:                  2024-01-20T15:14:37.691751-07:00
simple_title:         "Getting the current date"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Grabbing the current date in your code lets you stamp events right as they happen. It's key for logging, tracking time-sensitive data, and customizing user experiences based on the date.

## How to:
In Haskell, you get the current date using the `Data.Time` library. First, import what you need:

```haskell
import Data.Time
```

Now, snag today's date:

```haskell
main :: IO ()
main = do
    today <- getCurrentTime
    putStrLn $ "Today's date is: " ++ show (utctDay today)
```

Sample output might look like this:

```
Today's date is: 2023-03-23
```

## Deep Dive
Haskell's been doing date-time since its earlier days, the `Data.Time` library evolving from older time libraries. It's got what you need out of the box, but can be a bit intimidating. Alternatives exist, like `time-recurrence` for patterned date calculations, or `old-time`, Haskell's former go-to for date-time operations.

`Data.Time` works a lot with `UTCTime`, the universal time standard. But you can also deal with time zones using `ZonedTime` under the same library. It works by combining a `LocalTime` (date and time without a zone) and a `TimeZone` that specifies the offset from `UTC`.

## See Also
- "Learn You a Haskell" for time-related operations: [http://learnyouahaskell.com](http://learnyouahaskell.com/)
- Time zone handling in Haskell: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-LocalTime.html)
