---
title:                "Calculating a date in the future or past"
html_title:           "Haskell recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

Calculating a Date in the Future or Past: A Haskell Guide

## What & Why?
Calculating a date in the future or past is the process of determining a specific date based on a starting date and a set number of days. This is a common task for programmers as it allows for creating dynamic and time-based applications such as scheduling, reminder systems, and event planning.

## How to:
In Haskell, there are several ways to calculate a date in the future or past. One method is to use the ```addDays``` function from the ```Data.Time``` library. This function takes in a starting date and a number of days, and returns a new date after the specified number of days have been added.

Here's an example of using the ```addDays``` function to calculate a date 10 days in the future starting from a given date:

```Haskell
import Data.Time

main = do
  let startDate = fromGregorian 2021 7 10 -- July 10, 2021
      futureDate = addDays 10 startDate
  print futureDate -- 2021-07-20
```
Similarly, to find a date in the past, you can use a negative number of days in the ```addDays``` function.

Another approach is to use the ```TimeSpan``` package, which provides a ```DateSpan``` data type for handling dates. Here's an example of using ```DateSpan``` to calculate a date 15 days in the past starting from a given date:

```Haskell
import Data.Time
import TimeSpan

main = do
  let startDate = fromGregorian 2021 7 15 -- July 15, 2021
      pastDate = DateSpan (-15) startDate
  print $ getDate pastDate -- 2021-07-01
```

## Deep Dive:
Calculating dates in the future or past has been an essential task for programmers since the early days of computing. Some early programming languages, like COBOL, had built-in features for handling dates. However, these features often had limitations and were not very flexible.

In Haskell, there are many libraries and packages available for handling dates. Some popular options include ```Data.Time```, ```TimeSpan```, and ```time-lens```. Each of these has its unique features and methods for calculating dates.

Besides using libraries, some programmers prefer to write custom functions for calculating dates. This approach gives them more control and allows for customization based on their specific needs.

## See Also:
- [Haskell's official documentation for Data.Time library](https://hackage.haskell.org/package/time/docs/Data-Time-Calendar.html)
- [The TimeSpan package on Hackage](https://hackage.haskell.org/package/TimeSpan)
- [Haskell library for dealing with dates and times](https://hackage.haskell.org/package/time-lens)