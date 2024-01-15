---
title:                "Calculating a date in the future or past"
html_title:           "Go recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past can be a useful tool for a variety of applications, from scheduling appointments to creating reminders. It allows for dynamic and flexible date manipulation, making it a valuable skill for any programmer to have.

## How To

To calculate a date in the future or past, we will use the `time` package in Go. First, we will need to import the package:

```
import "time"
```

Next, we will use the `time.Now()` function to get the current date and time:

```
currentDate := time.Now()
```

To calculate a date in the future, we can use the `AddDate()` function and specify the number of years, months, and days we want to add to our current date:

```
futureDate := currentDate.AddDate(1, 0, 0) // adds 1 year to currentDate
```

To calculate a date in the past, we can use the `AddDate()` function again, but with negative values:

```
pastDate := currentDate.AddDate(0, -6, 0) // subtracts 6 months from currentDate
```

We can then format the dates using the `Format()` function and specifying the desired layout. For example:

```
futureDateFormatted := futureDate.Format("01/02/2006")
```

This will format our future date as "Month/Day/Year" (e.g. 09/23/2022). Here's the full code:

```
import "time"

currentDate := time.Now()
futureDate := currentDate.AddDate(1, 0, 0)
futureDateFormatted := futureDate.Format("01/02/2006")

// output: 09/23/2022
fmt.Println(futureDateFormatted)
```

And here's the code for calculating a date in the past:

```
// subtracts 6 months from current date
pastDate := currentDate.AddDate(0, -6, 0)
pastDateFormatted := pastDate.Format("01/02/2006")

// output: 03/23/2021
fmt.Println(pastDateFormatted)
```

## Deep Dive

When using the `AddDate()` function, it's important to note that the parameters represent years, months, and days respectively, not just one specific unit. For example, if we want to subtract 1 month from a date, we would add "-1" in the "months" parameter, and not in the "days" parameter.

Also, you may have noticed that we used "01/02/2006" as the format layout in our examples. This is because Go follows a specific date and time layout convention:

- Month: 01
- Day: 02
- Year: 2006
- Hour: 15 (24-hour format)
- Minute: 04
- Second: 05
- PM: PM

By following this layout convention, we can easily manipulate and format our dates in Go.

## See Also

- [Time Package in Go](https://golang.org/pkg/time/)
- [Formatting Dates and Times in Go](https://yourbasic.org/golang/format-parse-string-time-date-example/) 
- [Date Manipulation in Go](https://www.calhoun.io/working-with-date-and-time-in-go/)