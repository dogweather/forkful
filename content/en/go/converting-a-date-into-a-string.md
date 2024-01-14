---
title:                "Go recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
When working with dates in programming, it is often necessary to convert them into strings for various purposes such as display or manipulation. In this blog post, we will explore how to convert a date into a string in Go, a popular programming language known for its efficiency and concurrency.

## How To
Converting a date into a string in Go is a relatively simple process. First, we need to import the `time` package which contains the necessary functions for working with dates and times. Once we have imported the package, we can use the `Format` function to convert the date into a string. Let's take a look at an example:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Creating a date using the Date function
    date := time.Date(2020, time.May, 14, 0, 0, 0, 0, time.UTC)

    // Converting the date into the desired string format
    dateString := date.Format("01-02-2006")

    fmt.Println(dateString) // Output: 05-14-2020
}
```

In this example, we create a new date using the `Date` function from the `time` package. Then, we use the `Format` function to convert the date into a string in the format of "month-day-year". The output shows the date in the desired string format.

We can also use other characters in the format to customize the string output. For example, if we change the format to "Monday, Jan 2, 2006", the output will be "Thursday, May 14, 2020". The characters used in the format have special meanings, and you can find a complete list of them in the official Go documentation for the `time` package.

## Deep Dive
Behind the scenes, the `Format` function uses the `type Struct` and `func (t Time) Format(layout string) string` to convert the date into a string. The `Struct` type contains the year, month, day, hour, minute, second, and time zone information of a date, allowing for easy manipulation and conversion. The `Format` function then uses a predefined layout to format the date according to the developer's specifications.

It is worth noting that the `Format` function is not limited to converting dates into strings. It can also be used to format times, time zones, and various other date and time-related values.

## See Also
- [Go `time` package documentation](https://golang.org/pkg/time/)
- [Go Playground with example code](https://play.golang.org/p/mrWLTyM0FWo)
- [Overview of Go's string formatting options](https://yourbasic.org/golang/string-formatting-guide/)
- [Working with dates and times in Go](https://blog.golang.org/go-time)