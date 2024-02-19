---
aliases:
- /en/elm/getting-the-current-date/
date: 2024-01-20 15:13:49.092090-07:00
description: "Getting the current date in Elm means fetching the current calendar\
  \ date from the system. We do this to timestamp events, schedule tasks, or track\u2026"
lastmod: 2024-02-18 23:09:10.982167
summary: "Getting the current date in Elm means fetching the current calendar date\
  \ from the system. We do this to timestamp events, schedule tasks, or track\u2026"
title: Getting the current date
---

{{< edit_this_page >}}

## What & Why?
Getting the current date in Elm means fetching the current calendar date from the system. We do this to timestamp events, schedule tasks, or track durations.

## How to:
Elm handles dates with the `Time` module. You'll get the current time as a POSIX timestamp, then convert to a date.

```Elm
import Browser
import Task
import Time

type Msg = GetCurrentTime Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GetCurrentTime posixTime ->
            let
                -- Convert POSIX time to a date record
                date = Time.toDate posixTime
            in
            -- Update your model accordingly here
            ({ model | date = date }, Cmd.none)

-- To initiate getting the current time
getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform GetCurrentTime Time.now

-- Example output:
-- date { year = 2023, month = Mar, day = 26 }
```

## Deep Dive
In older web languages, grabbing the date is one-liner code. Elm is different. It makes side-effects like getting the current time explicit through the Elm Architecture. This encourages purity and maintainability of code.

Alternatives include using third-party packages or handling dates in your server code and passing them to Elm through flags or ports.

Implementation-wise, Elm's `Time.now` gets the time as a POSIX timestamp (milliseconds since Unix epoch). This is timezone-agnostic, and you can format it as needed using functions from the `Time` module.

## See Also
- [Elm Time documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm's guide to commands and subscriptions](https://guide.elm-lang.org/effects/)
