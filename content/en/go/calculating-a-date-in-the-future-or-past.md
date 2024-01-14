---
title:                "Go recipe: Calculating a date in the future or past"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past is a common task in many programming languages. In Go, it can be especially useful for time-based operations and scheduling tasks.

## How To

To calculate a date in the future or past in Go, we can use the `time` package. This package provides various methods for handling time and dates.

First, we need to import the `time` package into our code:

```
import "time"
```

Next, we can use the `Now()` method to get the current time:

```
now := time.Now()
```

To calculate a date in the future, we can use the `Add()` method and specify a duration in seconds, minutes, hours, days, or even years:

```
futureDate := now.Add(24 * time.Hour) // adds 24 hours to current date
```

Similarly, to calculate a date in the past, we can use the `Sub()` method:

```
pastDate := now.Sub(7 * time.Day) // subtracts 7 days from current date
```

We can then use the `Format()` method to format the date according to our desired layout:

```
futureDateFormatted := futureDate.Format("Monday, January 2, 2006")
```

Here, the layout string represents the desired format of the date and time, based on a specific reference date of "January 2, 2006".

Let's see the output of these calculations:

```
Future date: Monday, January 26, 2021
Past date: Monday, January 11, 2021
```

## Deep Dive

When calculating dates, it's important to take into account time zones and daylight saving time. In Go, we can use the `Location` and `LoadLocation()` methods to handle time zones.

Another helpful tip is to use the `Duration` type instead of manually calculating durations. This allows for more readable and maintainable code.

Furthermore, for more advanced date and time calculations, we can use the `time.ParseDuration()` method to specify a duration string in a specific format.

## See Also

- [Go `time` package documentation](https://golang.org/pkg/time/)
- [Working with time in Go](https://opensource.com/article/20/7/time-go)
- [Understanding time zones in Go](https://golangbyexample.com/golang-get-timezone-offset/)