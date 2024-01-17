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

## What & Why?
Calculating a date in the future or past refers to the process of manipulating dates by adding or subtracting a certain number of days, months, or years. 
Programmers often need to do this in order to perform tasks such as scheduling events, calculating deadlines, or displaying dates in a user-friendly format.

## How to:
In Go, the standard library provides the `time` package which offers various functions and methods for working with dates and times. Here are some examples of how to calculate a date in the future or past using Go:

```Go
// Calculating a date 10 days from now
futureDate := time.Now().AddDate(0, 0, 10) 
fmt.Println(futureDate) // Output: 2021-10-25 16:45:00 -0400 EDT

// Calculating a date 3 months and 2 days from now 
futureDate := time.Now().AddDate(0, 3, 2)
fmt.Println(futureDate) // Output: 2022-01-27 16:45:00 -0500 EST 

// Calculating a date 5 years from now 
futureDate := time.Now().AddDate(5, 0, 0)
fmt.Println(futureDate) // Output: 2026-10-16 16:45:00 -0400 EDT

// Calculating a date 2 weeks ago
pastDate := time.Now().AddDate(0, 0, -14)
fmt.Println(pastDate) // Output: 2021-10-02 16:45:00 -0400 EDT

// Calculating a date 6 months ago
pastDate := time.Now().AddDate(0, -6, 0)
fmt.Println(pastDate) // Output: 2021-04-16 16:45:00 -0400 EDT
```

## Deep Dive:
The concept of calculating dates is not new and has been used in various calendars and programming languages throughout history. In fact, some calendars have complex algorithms for calculating dates due to the complexities of their systems.

There are also other ways to calculate dates in addition to the `time` package in Go, such as using third-party libraries or manually manipulating the underlying date values.

The `time` package in Go implements a proleptic Gregorian calendar, meaning that it applies the rules of the Gregorian calendar (used in modern times) to dates before it was introduced. This can lead to some unexpected results, such as calculating a date before the year 1582 (when the Gregorian calendar was adopted).

## See Also:
- Official Go documentation for `time` package: https://golang.org/pkg/time/
- A tutorial on working with dates and times in Go: https://golangbot.com/datetime
- An in-depth explanation of the `time` package in Go: https://www.calhoun.io/working-with-dates-and-times-in-go/