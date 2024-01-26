---
title:                "Comparing two dates"
date:                  2024-01-20T17:33:00.160473-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparing two dates"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparing two dates means figuring out which one is earlier or how much time is between them. Programmers do it to handle stuff like deadlines, schedules, or time-based features.

## How to:

Elm makes date comparisons straightforward. Let's say you've got two dates. Here's how you'd check which one comes first:

```Elm
import Time exposing (Posix)
import Date

compareDates : Posix -> Posix -> Order
compareDates date1 date2 =
    if date1 < date2 then
        LT  -- date1 is earlier than date2
    else if date1 > date2 then
        GT  -- date1 is later than date2
    else
        EQ  -- dates are the same

-- Sample Usage:
let
    date1 = Date.fromPosix <| Time.millisToPosix 1650931200000 -- Add your first date in POSIX time
    date2 = Date.fromPosix <| Time.millisToPosix 1651017600000 -- And your second date in POSIX time
in
compareDates date1 date2
-- Output will be either LT, GT, or EQ
```

You can also calculate the difference in milliseconds:

```Elm
timeDifference : Posix -> Posix -> Time.Duration
timeDifference date1 date2 =
    Time.millisToPosix date1 - Time.millisToPosix date2

-- Sample Usage:
let
    date1 = Date.fromPosix <| Time.millisToPosix 1650931200000
    date2 = Date.fromPosix <| Time.millisToPosix 1651017600000
in
timeDifference date1 date2
-- Output: Duration in milliseconds
```

## Deep Dive
Elm stores dates as `Posix`, representing milliseconds since Unix epoch (1 January 1970, UTC). This is a common approach, sharing its roots with Unix Time, and it eases date manipulation and storage. 

While Elm's core library provides basic date handling, some alternatives like `justinmimbs/date` exist for more complex operations.

When implementing date comparisons, remember time zones can complicate things. Elm's `Time` module assumes UTC, which means you're spared daylight saving headaches, but you might need to adjust for local time zones in your application.

## See Also
- Elm Time module: https://package.elm-lang.org/packages/elm/time/latest/
- Justin Mimbs' Date package for Elm: https://package.elm-lang.org/packages/justinmimbs/date/latest/
- Unix Time: https://en.wikipedia.org/wiki/Unix_time
