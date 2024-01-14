---
title:    "Go recipe: Getting the current date"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

As programmers, we often need to fetch the current date and time for various tasks, such as logging, scheduling events, or displaying information to users. In this blog post, we will explore how to get the current date using the Go programming language.

## How To

First, we need to import the `time` package, which contains functions for working with dates and times.

```Go
import "time"
```

To get the current date, we can use the `Now()` function from the `time` package, which returns a `Time` value representing the current date and time.

```Go
currentDate := time.Now()
```

We can then format this date into a specific format using the `Format()` method, which takes in a specified layout.

```Go
currentDateFormatted := currentDate.Format("January 02, 2006")
```

Let's print out the current date to see the result.

```Go
fmt.Println(currentDateFormatted)
```

The output will be in the format we specified in the `Format()` method: "Month Day, Year". For the current date, the output will be "November 17, 2021".

## Deep Dive

Behind the scenes, the `Now()` function uses the system's timezone to get the current date and time. If you need to work with a different timezone, you can use the `LoadLocation()` function from the `time` package.

```Go
time.LoadLocation("Asia/Tokyo")
```

This will return a `*Location` value, which can be used in the `Now()` function to get the current date and time for that specific timezone.

```Go
japanDate := time.Now().In(location)
```

Additionally, the `time` package also has functions for manipulating dates and times, such as adding or subtracting time, comparing dates, and extracting specific parts of a date (such as the day, month, or year).

Now that you know how to get the current date in Go, you can explore these other functions to further manipulate date and time values in your programs.

## See Also

Here are some helpful resources for working with dates and times in Go:

- [Official Go documentation on the `time` package](https://golang.org/pkg/time/)
- [A detailed tutorial on working with dates and times in Go](https://www.golangprograms.com/go-language/date-time.html)
- [A cheat sheet for formatting dates and times in Go](https://programming.guide/go/cheat-sheet-for-mysql-and-golang-in-time-formatting-parsing.html)

Happy coding!