---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to compare two dates in your Elm program? Maybe you want to check if one date is before or after another, or determine the time difference between them. Whatever the reason may be, knowing how to compare dates in Elm can come in handy in various scenarios.

## How To

To compare two dates in Elm, we can use the `Time` module's `since` function. This function takes in two `Posix` values, representing the number of milliseconds since January 1, 1970. We can use the `Date.fromTime` function to convert our `Date` values into `Posix` values. Let's see an example:

```Elm
import Time exposing (Posix, since)
import Date exposing (fromTime)

-- creating two Date values to compare
date1 : Date
date1 = Date.fromTime 1622336400000

date2 : Date
date2 = Date.fromTime 1622422800000

-- comparing dates using since function
compare : Ordering
compare = since date1 date2
```

In this example, we create two `Date` values and use the `since` function to compare them. The `since` function returns an `Ordering` value, which can be `LT` (less than), `GT` (greater than), or `EQ` (equal). 

We can also use the `diffInDays` function from the `Date` module to find the number of days between two dates:

```Elm
import Date exposing (diffInDays)

-- finding the number of days between two dates
days : Int
days = diffInDays date1 date2
```

The `diffInDays` function returns an `Int` value representing the number of days between the two dates.

## Deep Dive

Behind the scenes, the `Date.fromTime` function uses the `Time.utcToPosix` function, which converts a `Time` value into a `Posix` value. This conversion takes into account time zones, so it's important to make sure your dates are in the same time zone before comparing them.

It's also worth noting that the `Time` module has other useful functions for working with dates and times, such as `sinceNow` and `inHours`. These functions can be handy when dealing with time-sensitive tasks in your Elm programs.

## See Also

- [The Date module in Elm's documentation](https://package.elm-lang.org/packages/elm/time/latest/Date)
- [The Time module in Elm's documentation](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [A tutorial on Time in Elm by DailyDrip](https://www.dailydrip.com/blog/elm-time-primer-building-a-stopwatch)