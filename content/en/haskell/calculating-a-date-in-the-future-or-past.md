---
title:                "Haskell recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often need to determine a specific date in the future or past. This could be for scheduling events, calculating deadlines, or even just for curiosity. Calculating dates in Haskell can be a useful skill to have, and it's surprisingly simple to do.

## How To

First, we need to import the `Data.Time` module, which provides functions and types for dealing with dates and times.

```Haskell
import Data.Time
```

Next, we can create a function that takes in a specific date and the number of days we want to add to that date:

```Haskell
addDaysToDate :: Day -> Integer -> Day
addDaysToDate date days = addDays days date
```

This function uses the `addDays` function from the `Data.Time` module to add the specified number of days to the given date. We can then use this function to calculate a date in the future:

```Haskell
futureDate = addDaysToDate (fromGregorian 2021 05 10) 30
-- Output: 2021-06-09
```

In this example, we are adding 30 days to the 10th of May in 2021, and the result is the 9th of June in 2021.

We can also calculate a date in the past by passing in a negative number of days:

```Haskell
pastDate = addDaysToDate (fromGregorian 2021 05 10) (-30)
-- Output: 2021-04-10
```

Here, we are subtracting 30 days from the 10th of May in 2021, resulting in the 10th of April in 2021.

## Deep Dive

In Haskell, dates are represented using the `Day` type, which is a type synonym for `DiffTime`. This type represents the number of days since the Modified Julian Day (MJD) epoch. The `Data.Time` module provides various functions for converting between different date representations, such as `fromGregorian` which takes in year, month, and day values and returns a `Day` type.

It's worth noting that the `addDays` function can also handle negative numbers, making it easy to calculate dates in the past. However, it does have some limitations when it comes to handling leap years. For more precise calculations, the `addGregorianMonthsClip` function can be used instead.

## See Also

- [Data.Time documentation](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskell.org](https://www.haskell.org/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/)