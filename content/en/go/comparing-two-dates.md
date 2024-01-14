---
title:    "Go recipe: Comparing two dates"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

When working with dates, it is often necessary to compare them in order to determine which one is larger or if they are equal. This is particularly useful in applications where the scheduling or ordering of events is important. In this blog post, we will explore how to compare two dates in Go and handle the results.

## How To

In Go, dates are represented using the `time` package. To compare two dates, we will need to use the `time.Time` type and its `After()`, `Before()`, and `Equal()` methods. Let's take a look at some examples:

```
package main

import (
    "fmt"
    "time"
)

func main() {
    // Create two dates
    date1 := time.Date(2020, 01, 01, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2020, 01, 02, 0, 0, 0, 0, time.UTC)

    // Compare using After()
    fmt.Println(date1.After(date2))  // false
    fmt.Println(date2.After(date1))  // true

    // Compare using Before()
    fmt.Println(date1.Before(date2)) // true
    fmt.Println(date2.Before(date1)) // false

    // Compare using Equal()
    fmt.Println(date1.Equal(date2))  // false
    fmt.Println(date1.Equal(date1))  // true
}

```

As you can see, the `After()` method returns `true` if the first date comes after the second date, `Before()` returns `true` if the first date comes before the second date, and `Equal()` returns `true` if the two dates are equal.

To make it easier to compare dates, we can also use the `time.Parse()` function to convert strings into `time.Time` values. For example:

```
dateStr := "2020-01-01"
date, _ := time.Parse("2006-01-02", dateStr) // The format must be "2006-01-02" for this to work
```

Additionally, we can also use the `time.ParseDuration()` function to convert time durations to `time.Duration` values, making it possible to compare time intervals as well.

## Deep Dive

When comparing dates in Go, it is important to note that the `time.Time` type is not only defined by the date but also by the timezone. This means that two dates from different timezones may not be equal even if they represent the same point in time. In such cases, it may be necessary to convert both dates to the same timezone before comparing them.

Another thing to keep in mind is that the `time.Time` type also includes the concept of leap seconds, which are not always considered in other languages. This can lead to unexpected results when comparing dates.

## See Also

To learn more about comparing dates in Go, check out the official documentation for the `time` package: https://golang.org/pkg/time/.

You can also refer to the following resources for further reading:

- https://yourbasic.org/golang/compare-dates/
- https://www.alexedwards.net/blog/making-and-comparing-time-with-golang
- https://medium.com/@arpit/working-with-time-and-date-in-golang-8a9fbc4448a

Happy coding!