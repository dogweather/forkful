---
date: 2024-01-20 17:30:39.440942-07:00
description: 'How to: Elm''s `Time` module and the `justinmimbs/time-extra` package
  let us mess with dates easily.'
lastmod: '2024-03-13T22:45:00.022917-06:00'
model: gpt-4-1106-preview
summary: Elm's `Time` module and the `justinmimbs/time-extra` package let us mess
  with dates easily.
title: Calculating a date in the future or past
weight: 26
---

## How to:
Elm's `Time` module and the `justinmimbs/time-extra` package let us mess with dates easily.

```Elm
import Time exposing (Posix)
import Time.Extra as TimeExtra

--calculateDate : Int -> Posix -> Posix
-- @deltaDays: number of days to add (negative to subtract)
-- @fromDate: starting date in Posix format

calculateDate deltaDays fromDate =
    TimeExtra.add TimeExtra.days deltaDays fromDate

-- Usage
-- Don't forget, Elm counts time in milliseconds since Unix epoch.

sampleDate = Time.millisToPosix 1580515200000  -- February 1, 2020 00:00:00 UTC
futureDate = calculateDate 10 sampleDate       -- Adds 10 days
pastDate = calculateDate -15 sampleDate        -- Subtracts 15 days

-- sample outputs:
-- futureDate -> 1581552000000  -- February 12, 2020 00:00:00 UTC
-- pastDate -> 1580006400000    -- January 17, 2020 00:00:00 UTC
```

## Deep Dive
Back in the day, dealing with dates in programming was a pain. Different systems, formats, and time zones gave everyone a headache. Elm's `Time` module, based on the Unix Time system (milliseconds since 1970), standardizes this. The `justinmimbs/time-extra` package further simplifies handling operations on dates, like adding or subtracting days.

Alternatives? Other languages have their own libraries, like Python's `datetime` or JavaScript's `Date`. But Elm's approach offers strong typing and purity, reducing bugs.

Beyond adding days, you can also work with months, years, or even hours and minutes. The functions in Elm and in packages like `time-extra` focus on immutability and pure functions—this means no side effects. When you calculate a new date, the original stays unchanged.

## See Also
- Elm `Time` module: https://package.elm-lang.org/packages/elm/time/latest/
- `justinmimbs/time-extra` package: https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/
- Elm Guide on Time: https://guide.elm-lang.org/effects/time.html
