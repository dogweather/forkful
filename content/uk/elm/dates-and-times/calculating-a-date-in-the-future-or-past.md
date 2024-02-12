---
title:                "Обчислення дати у майбутньому або минулому"
aliases: - /uk/elm/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:31:04.186687-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
## Що і чому?

Calculating a future or past date involves figuring out a date some days ahead or behind a specific date. Coders need this for features like reminders, subscription renewals, or historical data analysis.

## How to:
## Як це робити:

Elm makes date computations quite clear. Let's calculate today's date plus 10 days and minus 30 days.

```Elm
import Time
import Date exposing (Date)
import Task
import Browser

type Msg
    = GotTime Posix

type alias Model =
    { today : Maybe Date
    , plusTen : Maybe Date
    , minusThirty : Maybe Date
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( { today = Nothing, plusTen = Nothing, minusThirty = Nothing }
    , Time.now |> Task.perform GotTime
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTime posix ->
            let
                today = posix |> Time.toDate
                plusTen = Date.add Days 10 today
                minusThirty = Date.add Days -30 today
            in
            ( { model | today = Just today, plusTen = Just plusTen, minusThirty = Just minusThirty }
            , Cmd.none
            )

-- Add your view and subscriptions if needed, for a complete program

```

Sample Output:

```Elm
Model
    { today = Just (Date 2023-03-25)
    , plusTen = Just (Date 2023-04-04)
    , minusThirty = Just (Date 2023-02-23)
    }
```

## Deep Dive
## Поглиблений розгляд

Elm's handling of dates hinges on the `Time` and `Date` modules. Historically, date manipulation in programming is tricky due to time zones and leap years. Elm simplifies this by using `Posix` time, counting milliseconds since the Unix epoch.

Alternatives? You could use JavaScript's `Date`, but it has issues around daylight saving time and mutability. Elm's `Date` is immutable and handles complexities silently.

For in-depth date logic, like recurring events, you might need a custom solution. Elm's simplicity also means some use cases require extra effort, like installing third-party date libraries.

## See Also
## Дивись також

- Elm's [official Time documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [rtfeldman/elm-iso8601-date-strings](https://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest/) for ISO 8601 string parsing
