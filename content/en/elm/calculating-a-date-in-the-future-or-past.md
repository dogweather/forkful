---
title:                "Calculating a date in the future or past"
html_title:           "Elm recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a future or past date means going forward or backward in the timeline from a specific day. Programmers do it for plenty of reasons, like scheduling events, setting deadlines, or managing subscriptions.

## How To:

Here is how we do date calculation in Elm with its `Time` module:

```Elm
import Time exposing (..)

-- Define base date: 1st Jan, 2021
let
    baseDate = 
        Time.millisToPosix 1609459200000

-- Define duration: 100 days
let
    durationDays = 
        Time.daysToMillis 100

-- Future date after 100 days
let 
    futureDate = 
        Time.plus baseDate durationDays 

-- Print Future date
Debug.log "Future date " futureDate
```

This small Elm code will give you the future date after 100 days from 1st Jan, 2021. Similar subtraction can be used to calculate past dates.

## Deep Dive

A brief history: Date calculation was always crucial in scheduling tasks, but coding it used to be a mess, dealing with odd month lengths, leap years, and timezones. Elm abstracted all this with its `Time` module, following Unix's approach to managing dates as milliseconds since the 'Epoch' (1st Jan, 1970).

Alternatives: While Elm provides native date calculation, you can also use libraries like `rtfeldman/elm-iso8601-date-strings` for more advanced features at the cost of external dependencies.

Implementation Details: Calculation in Elm is straightforward. Days are converted to milliseconds (suitable for the Posix Time format) and simply added to or subtracted from the base date. This is internally managed by Elm's runtime which makes it consistent across platforms.

## See Also

- [Elm Time Module Documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- Elm's date library: [rtfeldman/elm-iso8601-date-strings](https://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest/)