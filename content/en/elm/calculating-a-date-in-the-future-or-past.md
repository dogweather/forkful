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

Calculating a date in the future or past refers to the process of determining a future or past date based on a given starting date and a specific number of days. Programmers often do this in order to automate tasks such as scheduling events, creating reminders, or calculating deadlines.

## How to:

To calculate a date in the future or past in Elm, you can use the `addDays` function from the `Time` library. This function takes in two arguments: the number of days to add (or subtract) and the starting date. Here's an example of how you can use it:

```Elm
import Time exposing (..)

start = fromCalendarDate 2020 7 15
daysToAdd = 30
futureDate = addDays daysToAdd start

-- futureDate is now August 15th, 2020
```

You can also use negative values to subtract days, as shown in this example:

```Elm
import Time exposing (..)

start = fromCalendarDate 2020 7 15
daysToSubtract = -10
pastDate = addDays daysToSubtract start

-- pastDate is now July 5th, 2020
```

## Deep Dive:

Historically, calculating dates in the future or past has been a common problem for programmers. Before specific libraries and functions were created to handle this task, programmers had to write their own algorithms to determine future or past dates. This often involved complex calculations and checking for things such as leap years and varying months.

In Elm, the `addDays` function simplifies the process significantly. However, there are alternative approaches, such as using the `Time` modules' `toTime` function to convert to Unix time and then using the `Time.fromPosix` function to convert back to a date.

For those interested in the implementation details, the `Time` library uses the Gregorian calendar to calculate dates. This means that it takes into account leap years, varying month lengths, and other nuances that come with tracking time.

## See Also:

- [Documentation for `Time` library in Elm](https://package.elm-lang.org/packages/elm/time/latest/)
- [Using Elm's `Time` library for timezone calculations](https://medium.com/@danbruder/using-elms-time-library-for-timezone-calculations-b3c936513808)
- [Overview of historical calendar and time calculations](https://fetchelmdocs.com/blog/Elm/Introducing-CalendarHs-a-Date-Calculating-Library-for-Elm)