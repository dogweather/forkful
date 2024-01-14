---
title:                "Haskell recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing dates may seem like a simple task, but it can quickly become complicated when dealing with different formats and time zones. However, being able to compare dates is essential for many programming tasks, such as scheduling events or filtering data.

## How To

In Haskell, there are several libraries that allow for easy date and time comparisons. One popular option is the `time` library, which provides functions for creating and manipulating dates. Let's take a look at some code examples to see how we can compare dates using this library.

First, we need to import the `Data.Time` module:

```Haskell
import Data.Time
```

Next, we can create two `Day` values using the `fromGregorian` function. This function takes in the year, month, and day as parameters, and returns a `Day` value.

```Haskell
let date1 = fromGregorian 2020 1 1
let date2 = fromGregorian 2021 1 1
```

Now, we can use the `compare` function to compare these two dates. This function returns an `Ordering` value, which can be `LT` (less than), `GT` (greater than), or `EQ` (equal).

```Haskell
compare date1 date2 -- Output: LT
```

We can also use the `diffDays` function to find the number of days between two dates.

```Haskell
diffDays date1 date2 -- Output: -365
```

In addition, the `time` library also provides functions for comparing times, time zones, and daylight saving time. Refer to the library documentation for more details and examples.

## Deep Dive

Comparing dates can become more complex when dealing with different time zones, daylight saving time, or leap years. To ensure accurate comparisons, it is important to use a library that handles these nuances correctly.

The `time` library also offers functions for handling these scenarios. For example, the `UTCTime` type takes into account leap seconds, and the `ZonedTime` type allows for converting between time zones. Make sure to thoroughly test your code and consider edge cases when comparing dates.

## See Also

- [Haskell Documentation - Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Real World Haskell - Dates and Times](http://book.realworldhaskell.org/read/numbers.html#id642958)
- [Learn You a Haskell - Making Our Own Types and Typeclasses](http://learnyouahaskell.com/making-our-own-types-and-typeclasses#time)