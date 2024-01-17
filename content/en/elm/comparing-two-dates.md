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

## What & Why?

When programming in Elm, it's common to encounter situations where we need to compare two dates. This simply means determining if one date is earlier, later, or equal to another one. Programmers do this to sort and organize chronologically ordered data, perform date-based calculations, or validate date inputs from users.

## How to:

To compare two dates in Elm, we can use the `Date.compare` function. It takes two dates as arguments and returns an `Ordering` value, which can be either `LT` (less than), `GT` (greater than), or `EQ` (equal).

```
import Date exposing (Date, compare)

date1 : Date
date1 = Date.fromString "2021-05-10"

date2 : Date
date2 = Date.fromString "2021-05-15"

result : Ordering
result = Date.compare date1 date2  -- result is GT since date2 is later than date1
```

We can also use the `Date.daySinceEpoch` function to get the number of days since January 1st, 1970, and then compare this value instead of directly comparing two dates.

```
import Date exposing (Date, daySinceEpoch)

date1 : Date
date1 = Date.fromString "2021-05-10"

date2 : Date
date2 = Date.fromString "2021-05-15"

result : Ordering
result = compare (daySinceEpoch date1) (daySinceEpoch date2)  -- result is GT
```

## Deep Dive:

Compared to other programming languages, Elm has a limited number of date functions available. This is due to the language's focus on immutability and pure functions. Other alternatives to compare dates include using the `Date.toTime` function to convert dates to `Time` values and then comparing them with the `Time.compare` function.

The `Date.compare` function in Elm uses the Unix timestamp system to represent dates as a number of milliseconds since January 1st, 1970. This means that dates before this date will be represented by negative numbers, and dates after it will be represented by positive numbers.

## See Also:

- Elm Date Documentation: https://package.elm-lang.org/packages/elm/core/latest/Date
- Elm Time Documentation: https://package.elm-lang.org/packages/elm/time/latest/Time
- QuickChart's Date Comparison Tool: https://quickchart.io/date-comparison/