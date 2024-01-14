---
title:                "Haskell recipe: Comparing two dates"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

When programming, it is often necessary to compare dates in order to determine the order or calculate the time difference between two events. In Haskell, dates are represented as data types and there are various functions available for comparing them. Understanding how to compare dates in Haskell can greatly enhance your ability to manipulate and analyze data.

## How To

To compare two dates in Haskell, we first need to import the Data.Time module. This module provides functions for working with different date and time data types.

```haskell
import Data.Time
```

Next, we need to create two date objects using the `fromGregorian` function, which takes in three parameters: year, month, and day.

```haskell
let date1 = fromGregorian 2021 10 15
let date2 = fromGregorian 2021 10 20
```

We can then use the `compare` function to compare the two dates. This function returns an `Ordering` type, which can be either `LT` (less than), `GT` (greater than), or `EQ` (equal).

```haskell
compare date1 date2 -- returns LT
```

To calculate the time difference between two dates, we can use the `diffDays` function. This function takes in two date objects and returns the difference in days as an `Int`.

```haskell
diffDays date1 date2 -- returns -5
```

## Deep Dive

Behind the scenes, dates in Haskell are represented using the `Day` data type, which is essentially an integer representing the number of days since the beginning of the Gregorian calendar. This allows for efficient comparison and manipulation of dates.

It is important to note that when comparing dates, time zones are not taken into account. This means that two dates may appear to be equal, but have a time difference due to being in different time zones. To accurately compare dates that include time zones, the `ZonedTime` data type should be used.

## See Also

- [Data.Time documentation](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Haskell Tutorial: Comparing Dates](https://www.haskell.org/tutorial/numbers.html#k-comparing-numbers)