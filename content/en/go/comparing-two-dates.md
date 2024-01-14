---
title:    "Go recipe: Comparing two dates"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
If you are a programmer, you have probably come across a situation where you need to compare two dates. Whether it's for a scheduling application, a booking system, or simply to ensure chronological order, comparing dates is a common task in programming. In this blog post, we will explore how to compare two dates in Go.

## How To
In Go, there are multiple ways to compare dates, depending on your specific needs. Let's look at some coding examples:

```
// First, we will need to import the "time" package
import "time"

// Creating two date variables
date1 := time.Date(2020, time.July, 15, 0, 0, 0, 0, time.UTC)
date2 := time.Date(2020, time.July, 20, 0, 0, 0, 0, time.UTC)

// Comparing dates using the "Equal" function
if date1.Equal(date2) {
    fmt.Println("The dates are equal")
} else {
    fmt.Println("The dates are not equal")
}

// Comparing dates using the "Before" function
if date1.Before(date2) {
    fmt.Println("Date 1 is before Date 2")
} else {
    fmt.Println("Date 2 is before Date 1")
}

// Comparing dates using the "After" function
if date1.After(date2) {
    fmt.Println("Date 1 is after Date 2")
} else {
    fmt.Println("Date 2 is after Date 1")
}
```

Output:
```
The dates are not equal
Date 1 is before Date 2
Date 2 is after Date 1
```

## Deep Dive
Go's "time" package offers more advanced functions for comparing dates, such as "BeforeDate" and "AfterDate" which also take into account the time zone. You can also compare dates by converting them to the time stamp format and using logical operators like ">" or "<". Additionally, there are features for checking if a date falls within a specific time range or determining the difference between two dates in terms of hours, days, months, etc.

It's important to note that when comparing dates, it is recommended to use the "Equal" or "Before"/"After" functions, as they handle time zones and location information automatically.

## See Also
- Go "time" package documentation: https://golang.org/pkg/time/
- Tutorial on comparing dates in Go: https://www.calhoun.io/comparing-time-in-go/ 
- Helpful stack overflow thread on comparing time zones in Go: https://stackoverflow.com/questions/23714638/comparing-time-in-different-time-zones-in-golang