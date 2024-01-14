---
title:    "Haskell recipe: Calculating a date in the future or past"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
Calculating dates in the future or past can be a useful tool for a variety of applications, such as scheduling events, managing deadlines, or creating reminders. It allows for efficient and precise planning, making it a valuable skill for any programmer to have in their toolbelt.

## How To
Using Haskell, we can easily calculate dates in the future or past by using the `addDays` function from the `Data.Time` module. The `addDays` function takes in an `Int` value representing the number of days to add or subtract from the current date.

```
import Data.Time

--Calculating 5 days in the future
futureDate :: Day
futureDate = addDays 5 today
--Where `today` is the current date

--Calculating 3 days in the past
pastDate :: Day
pastDate = addDays (-3) today
```

Running the `futureDate` and `pastDate` functions in GHCi will output the corresponding dates in a `Day` data type, which can be easily formatted using the `show` function.

```
> futureDate
2020-11-15

> pastDate
2020-10-18
```

## Deep Dive
In addition to adding or subtracting days, the `Data.Time` module also allows for calculations with other units of time such as weeks, months, and years. This is achieved by using the `addGigaseconds`, `addGregorianMonths`, and `addGregorianYears` functions respectively.

```
--Calculating 2 weeks in the future
futureWeek :: Day
futureWeek = addDays 14 today

--Calculating 6 months in the past
pastMonth :: Day
pastMonth = addGregorianMonths (-6) today

--Calculating 3 years in the future
futureYear :: Day
futureYear = addGregorianYears 3 today
```

Additionally, the `Data.Time.Calendar.OrdinalDate` module allows for calculations based on ordinal dates, which refers to the day of the year instead of the month.

## See Also
- [Official Haskell Documentation for `Data.Time` module](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Haskell Wiki page on working with dates and times](https://wiki.haskell.org/Working_with_time_and_dates)