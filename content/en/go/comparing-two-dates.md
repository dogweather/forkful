---
title:                "Comparing two dates"
html_title:           "Go recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? 
Comparing two dates in the context of programming refers to the process of determining the chronological order of two given dates. This is an important task for programmers as it allows for better data organization and analysis, making it easier to understand and work with date-based information. 

## How to:
Comparing two dates in Go is a straightforward task. The standard library provides the `time` package, which offers functions specifically designed for date and time related operations. Here's an example of comparing two dates using the `IsZero()` and `Before()` functions:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // creating two date objects
    date1 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2020, time.December, 1, 0, 0, 0, 0, time.UTC)

    // comparing the dates
    // using IsZero() to check if date2 was not initialized
    if date2.IsZero() {
        fmt.Printf("date2 is not initialized")
    } else if date1.Before(date2) {
        fmt.Printf("%v is before %v", date1, date2)
    } else if date2.Before(date1) {
        fmt.Printf("%v is before %v", date2, date1)
    } else {
        fmt.Printf("%v and %v are the same dates", date1, date2)
    }
}
```

This code snippet creates two date objects and then uses the `IsZero()` function to check if the second date was initialized or not. If it was not initialized, a message is displayed. Otherwise, the `Before()` function is used to compare the dates and the appropriate message is displayed.

Sample output:
```
2021-01-01 00:00:00 +0000 UTC is before 2020-12-01 00:00:00 +0000 UTC
```

## Deep Dive:
Compared to other programming languages, Go offers a more simplified and efficient way of comparing dates. This is thanks to its `time` package, which provides functions for various operations such as creating dates, parsing date strings, and comparing dates. Before the inclusion of this package in Go 1.1, developers had to rely on other packages or write their own code to handle date and time operations.

While the `Before()` function is commonly used for comparing dates, the `After()` and `Equal()` functions can also be used depending on the specific needs of the program. Additionally, Go also offers the `Date()` function which allows for the creation of custom dates by specifying the year, month, and day values.

## See Also:
- [The Go Programming Language](https://golang.org/) official website
- [The time Package](https://golang.org/pkg/time/) in the Go standard library documentation
- [A Tour of Go](https://tour.golang.org/welcome/1) interactive tutorial for learning Go