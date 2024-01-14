---
title:                "Go recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating future or past dates can come in handy for a variety of programming tasks. It allows you to schedule events, set reminders, and perform time-based calculations, making your programs more dynamic and user-friendly.

## How To

To calculate a date in the future or past in Go, we will be using the `time` package. First, we will declare a variable of type `time.Time` to represent the current date. Then, we can use the `AddDate()` function to add or subtract a certain number of years, months, or days from the current date.

Let's take a look at an example:

```
package main

import (
    "fmt"
    "time"
)

func main() {
    currentDate := time.Now() // current date
    futureDate := currentDate.AddDate(2, 6, 10) // adding 2 years, 6 months, and 10 days to current date
    pastDate := currentDate.AddDate(-1, 0, 0) // subtracting 1 year from current date

    fmt.Println("Current date:", currentDate)
    fmt.Println("Future date:", futureDate)
    fmt.Println("Past date:", pastDate)
}
```

Output:

```
Current date: 2021-06-12 13:00:00 +0000 UTC
Future date: 2023-12-22 13:00:00 +0000 UTC
Past date: 2020-06-12 13:00:00 +0000 UTC
```

The `AddDate()` function also handles leap years and adjusts the date accordingly. You can also use the `Sub()` function to subtract a specific duration of time from the current date.

## Deep Dive

If you want more control over the calculation, you can also use the `Date()` function to specify the year, month, and day instead of relying on the current date. This allows you to calculate future or past dates from a specific starting point.

Additionally, the `Add()` and `Sub()` functions allow you to add or subtract durations of time in smaller increments, such as seconds, minutes, or hours. This can be useful for more precise calculations.

Keep in mind that the `time` package uses the UTC (Coordinated Universal Time) time zone by default. If you want to use a different time zone, you can use the `Location()` function to set a specific time zone before the calculations.

## See Also

- [Go `time` package documentation](https://golang.org/pkg/time/)
- [Calculating past and future dates in Go](https://www.calhoun.io/adding-dates-and-times-in-go/)
- [Working with dates and time in Go](https://blog.teamtreehouse.com/go-date-time)