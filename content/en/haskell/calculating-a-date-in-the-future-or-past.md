---
title:    "Haskell recipe: Calculating a date in the future or past"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past can be a useful skill in many programming applications. For example, it can be used to create reminders, schedule tasks, or calculate deadlines.

## How To

To calculate a date in the future or past, we first need to import the `Data.Time` module in Haskell.

```
import Data.Time
```

Next, we can use the `addDays` function to add or subtract a specific number of days to a given date.

```
addDays :: Day -> Int -> Day
```

For example, to calculate the date 5 days from now, we can use the following code:

```
let currentDate = fromGregorian 2021 3 15 -- March 15th, 2021
let futureDate = addDays currentDate 5 -- March 20th, 2021
```

We can also use the `addGregorianMonthsClip` function to add or subtract a specific number of months to a given date.

```
addGregorianMonthsClip :: Day -> Integer -> Day
```

For example, to calculate the date 6 months from now, we can use the following code:

```
let currentDate = fromGregorian 2021 3 15 -- March 15th, 2021
let futureDate = addGregorianMonthsClip currentDate 6 -- September 15th, 2021
```

To calculate a date in the past, we can simply use a negative number as the second argument in these functions.

## Deep Dive

Under the hood, Haskell uses a type called `Day` to represent dates. This type is defined as the number of days since the Modified Julian Day (MJD), which is the number of days since November 17th, 1858. This allows for easy conversion between different calendar systems and accounts for leap years.

Additionally, the `Data.Time` module includes functions for working with different time zones and accounting for daylight saving time, making it a reliable tool for date calculations.

## See Also

- [Haskell Documentation for Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Stack Overflow: How to calculate difference of days between two dates?](https://stackoverflow.com/questions/29176076/how-to-calculate-difference-of-days-between-two-dates)