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

Comparing two dates means determining their relative chronological order, and it allows programmers to execute specific actions based on the time flow, like ordering events, checking for expired subscriptions, and calculating time spans.

## How to:

Let's see how to compare two dates in Elm programming. The Elm `Date` module provides essential functions to compare dates. Suppose we have two dates:

```Elm
date1 = Date.fromCalendarDate 2021 1 1
date2 = Date.fromCalendarDate 2022 1 1
```

To compare them, we use Elm’s built-in `compare` function:

```Elm
comparisonResult = compare date1 date2
```

This function returns `LT` if the first date is before the second, `GT` if it is after, and `EQ` if they are the same.

## Deep Dive

Elm's native functions for date comparison are quite simple and effective, but Elm wasn't always this practical. Many older programming languages don't have a native function to compare dates, and Elm has been designed to avoid such pitfalls.

The built-in `compare` function uses Unix timestamp internally for comparison. As an alternative, you could manually convert the dates to Unix timestamps and compare those, but using the built-in function is more convenient and less error-prone.

## See Also

- Core Elm `Date` module documentation: [https://package.elm-lang.org/packages/elm/time/latest/Date](https://package.elm-lang.org/packages/elm/time/latest/Date)
  
- Detailed guide on Elm date/time manipulation: [https://elmprogramming.com/dates-and-times.html](https://elmprogramming.com/dates-and-times.html)