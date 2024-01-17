---
title:                "Comparing two dates"
html_title:           "Haskell recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is the process of determining which of the two dates is earlier or later. Programmers often do this to sort a list of dates, find out the time difference between two dates, or validate user input.

## How to:

In Haskell, there are various ways to compare two dates depending on the specific use case. Here are two simple examples:

```
-- Using the default Ord instance for the Date type
compareDates :: Date -> Date -> Ordering
compareDates d1 d2 = compare d1 d2

-- Custom comparison function for comparing years
compareYears :: Date -> Date -> Ordering
compareYears d1 d2 = compare (year d1) (year d2)
```

Sample output:
```
compareDates (Date 2020 12 7) (Date 2021 1 1) -- LT
compareYears (Date 2020 12 7) (Date 2021 1 1) -- LT
```

## Deep Dive:

There are alternative ways to compare dates using functions like `diffDays` or `compareCalendarTime`, but they require additional libraries or conversions to the `Day` or `CalendarTime` types. It is important to note that dates are complex entities with varying formats and time zones, so it is recommended to use a well-tested library like `time` for accurate and reliable comparisons.

## See Also:

- [Haskell Time Library](https://hackage.haskell.org/package/time)
- [Date and Time in Haskell](https://wiki.haskell.org/Date_and_time)
- [Difference between two dates in Haskell](https://stackoverflow.com/questions/30799011/difference-between-two-dates-in-haskell)