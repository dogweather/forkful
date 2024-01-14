---
title:                "Elm recipe: Getting the current date"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Imagine you are working on a project where you need to keep track of the current date. Maybe you want to display the current date on your website or use it for some calculations. Whatever the reason may be, getting the current date in Elm is a useful skill to have.

## How To

Getting the current date in Elm is a simple process. The `Time` module provides us with a function called `now` which returns the current date in the `Posix` format. In order to use this function, we need to import the `Time` module in our Elm file.

```Elm
import Time exposing (now)
```

Once we have imported the `Time` module, we can use the `now` function to get the current date and assign it to a variable. Let's call this variable `currentDate`.

```Elm
currentDate = now
```

This will give us the current date in the `Posix` format. Now, we can convert this into a more readable format using the `Time` module's `toYearMonthDay` function.

```Elm
import Time exposing (now, toYearMonthDay)

currentDate = now

prettyDate = toYearMonthDay currentDate

```

If we print out `prettyDate` using `Debug.log`, we will get the current date in the `YYYY-MM-DD` format. Note that the `toYearMonthDay` function also takes into account the time zone and daylight savings, making it a reliable way to get the current date.

```Elm
Debug.log "Current Date" prettyDate
```

Output:

```
Current Date: 2021-07-01
```

## Deep Dive

Behind the scenes, the `now` function uses JavaScript's `Date.now()` method to get the current time. It then converts this time into the `Posix` format. The `Posix` format is a numerical representation of time, measured in milliseconds since January 1, 1970 UTC. This format makes it easier for computers to work with dates and times.

Additionally, the `Time` module also provides other useful functions to work with dates and times, such as `add`, `sub`, and `fromIsoString`. These functions allow us to perform calculations and manipulate dates in Elm.

## See Also

If you want to learn more about working with dates and times in Elm, check out these helpful resources:

- [Elm Time library documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [How to Format Dates in Elm](https://medium.com/@go_to_victor/how-to-format-dates-in-elm-443b0e4dd7c3)
- [A Guide to Dates and Times in Elm](https://www.gizra.com/content/dates-and-times-elm/)