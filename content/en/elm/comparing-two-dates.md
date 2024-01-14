---
title:                "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

When working with dates in programming, it's common to need to compare two dates. This could be for tasks such as finding the difference in time between two events or determining if one date falls before or after another. In this blog post, we'll explore how to compare two dates using the functional programming language, Elm.

## How To

To compare two dates in Elm, we will use the built-in `Time` library. First, we need to define our two dates as `Time.Posix` values. These values represent the number of seconds since January 1st, 1970. Let's say we have the following two dates:

```Elm
date1 : Time.Posix
date1 = Time.millisToPosix 1600000000

date2 : Time.Posix
date2 = Time.millisToPosix 1620000000
```

Once we have our dates defined, we can use the comparison operators (`<`, `>`, `<=`, `>=`) to compare them. For example, to check if `date1` is before `date2`, we would use the following code:

```Elm
date1 < date2
```

This will return a `Bool` value of `True` if `date1` is indeed before `date2`, and `False` if not.

We can also find the difference in time between the two dates by subtracting them. This will give us the number of seconds between the two dates. For example:

```Elm
date2 - date1
```

This will return the value `20000000`, representing 20000000 seconds between the two dates.

## Deep Dive

When comparing dates, it's important to keep in mind the precision. In Elm, `Time.Posix` values have millisecond precision, but not all dates have the same level of precision. For example, the `Time.now` function returns a `Time.Posix` value with millisecond precision, but some dates such as `Time.beginningOfYear` only have day precision.

It's also worth noting that time zones and daylight saving time can affect the results of date comparisons. To avoid these complications, it's recommended to work with `Time.Posix` values in UTC time.

## See Also

- [Elm's `Time` library documentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Comparing Dates in JavaScript](https://www.w3schools.com/js/js_dates_compare.asp)
- [Date and Time in Functional Programming (YouTube video)](https://www.youtube.com/watch?v=D14UrkyF0oQ)