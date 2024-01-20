---
title:                "Comparing two dates"
html_title:           "Arduino recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

In a nutshell, comparing two dates is figuring out whether one given date is earlier, later, or the same as another. As programmers, we often need this to sequence events, calculate time intervals, or trigger time-based actions in software.
  
## How to:

To compare dates in Haskell, we use the `Data.Time` library. Here's a basic example:

```Haskell
import Data.Time.Clock
import Data.Time.Calendar

main = do
    let date1 = fromGregorian 2020 1 1
        date2 = fromGregorian 2021 1 1
    print (compare date1 date2)
```

The `compare` function will simply return a value of type `Ordering`, which can be `LT`, `EQ`, `GT` meaning less than, equal, or greater than respectively.

When you run the above code, you will see: 

```Haskell
LT
```

This means `date1` (1 Jan 2020) is earlier than `date2` (1 Jan 2021).

## Deep Dive

Historically, time-and-date manipulation has been an important, though occasionally underestimated, part of programming. From the early days of UNIX timed scheduling to modern-day e-commerce sales ending at midnight, time matters.

There are alternatives to the Haskell's `Data.Time` library, like the older `calendarTime`, but `Data.Time` is more comprehensive and favored in modern use.

The `compare` function is actually part of the `Ord` class in Haskell, common to all data types that are "order-able". It's implemented by converting the dates to Julian Day Numbers (a continuous count of days since the beginning of the Julian Period). It's Haskell's under-the-hood way of conveniently telling which day came first.

## See Also

For more about the `Data.Time` library, check out the [Haskell Library Documentation](https://hackage.haskell.org/package/time). 

If the default `compare` function doesn’t fit, you may want to create a custom comparison function. [Haskell Documentation on Ord](https://learn.haskell.org/doc/classes/03.html) will help. Finally, for historical perspectives, this [Computer History Page on Time](http://www.computerhistory.org/fellowawards/hall/time/) is worth a visit.