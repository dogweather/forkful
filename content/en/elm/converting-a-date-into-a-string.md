---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a date to a string in Elm means transforming a date object into a format that is easier to read and interpret for humans. Programmers do this to simplify data presentation and enhance user experience.
  
## How to:
You can leverage Elm's `Date` and `Time` libraries to manipulate dates. Here is an example where we convert the current date into a string using `Date.toIsoString`.

```Elm
import Time exposing (Posix)
import Date

formatDate : Posix -> String
formatDate date = 
    date
    |> Time.toHour
    |> Date.fromTime
    |> Maybe.map Date.toIsoString
    |> Maybe.withDefault "invalid date"

main : Html msg
main =
    let
        date = Time.millisToPosix 1609459200000
    in
        text (formatDate date)
```
Output:
```Bash
2021-01-01
```

## Deep Dive
Historically, Elm initially lacked built-in capabilities for date/time manipulation. The community developed packages to cover this gap, with the top ones being `justinmimbs/date` and `elm/time`. These libraries are now commonly used due to their reliability and performance.

A couple of alternatives to `Date.toIsoString` are `Date.toRfc1123String` and `Date.toRfc3339String` from the `justinmimbs/date` package or the `Date.Extra.Formats` module from `rluiten/elm-date-extra`, allowing customization of format.

When using `Date.toIsoString`, a date is converted to a String following the ISO 8601 date format. Nonetheless, if the conversion process finds an invalid date, it will default to "invalid date".

## See Also

- Elm documentation: [Time](https://package.elm-lang.org/packages/elm/time/latest/) & [Date](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
- Library for additional formatting options: [rluiten/elm-date-extra](https://package.elm-lang.org/packages/rluiten/elm-date-extra/latest/Date-Extra-Formats)
- Differences between `toIsoString`, `toRfc1123String`, and `toRfc3339String`: [Date formats](https://www.w3.org/TR/NOTE-datetime)
- Elm Reddit discussion on [date and time manipulation](https://www.reddit.com/r/elm/comments/ah9onw/decoding_and_managing_dates_and_times_in_elm/)