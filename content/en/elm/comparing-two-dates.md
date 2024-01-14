---
title:                "Elm recipe: Comparing two dates"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
When working with dates in programming, it is common to need to compare them. This can be useful for sorting data, calculating time intervals, or checking if a date falls within a certain range. In this blog post, we will explore how to compare two dates in Elm and the importance of understanding date comparison in programming.

## How To
To start, we need to import the Date library in Elm. This will give us access to functions that allow us to work with dates. Then, we can create two date objects using the `Date.fromTime` function. This function takes in an Unix timestamp as an argument, which represents the number of milliseconds since January 1, 1970. Once we have our date objects, we can use the `Date.compare` function to compare them. This function takes in two dates and returns `LT` (less than), `EQ` (equal to), or `GT` (greater than) based on the comparison.

```
Elm

import Date exposing (..)

date1 = Date.fromTime 1587946800000
date2 = Date.fromTime 1588107600000

Date.compare date1 date2 -- returns LT
```

To compare dates by a specific component (year, month, day, etc.), we can use the `Date.compareByField` function. This function takes in a field and two dates, and returns the comparison of the specified field for the two dates. For example, to compare the year of two dates:

```
Elm

Date.compareByField Year date1 date2 -- returns LT
```

## Deep Dive
It is important to understand that dates are represented as numbers in programming. Each date is a specific number of milliseconds since a specific reference date (January 1, 1970 in Unix time). This means that when we compare two dates, we are essentially comparing two numbers. This can lead to unexpected results if we are not careful.

For example, if we compare two dates without specifying a field, the comparison will be based on the entire date object, meaning it will take into account not only the year, but also the month, day, hour, minute, and second. This can lead to unexpected results, especially when comparing dates from different time zones.

To avoid these issues, it is important to specify a specific field when comparing dates. This ensures that the comparison is only based on the specified component, rather than the entire date object.

## See Also
- Elm Date Library Documentation: [https://package.elm-lang.org/packages/elm/time/latest/Date](https://package.elm-lang.org/packages/elm/time/latest/Date)
- Unix Time Converter: [https://www.unixtimestamp.com](https://www.unixtimestamp.com)
- Time and Date Comparison in Programming: [https://www.geeksforgeeks.org/time-and-date-comparison-in-programming/](https://www.geeksforgeeks.org/time-and-date-comparison-in-programming/)