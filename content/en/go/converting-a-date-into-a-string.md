---
title:    "Go recipe: Converting a date into a string"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to display a date in a specific format or manipulate it as a string in your Go program? Converting a date into a string can be useful in many scenarios, such as displaying a date on a webpage or parsing it from a CSV file. In this blog post, we will explore how to convert a date into a string in the Go programming language.

## How To

To convert a date into a string, we first need to define our date in a variable. We can use the `time` package in Go to create a `time.Time` object, which represents a specific moment in time. Let's say we have a date of September 13th, 2021:

```Go
date := time.Date(2021, time.September, 13, 0, 0, 0, 0, time.UTC)
```

Next, we can use the `Format()` method on our `time.Time` object to specify the desired format for our date string. For example, if we want to display the date as "Monday, January 2, 2006", we would use the following code:

```Go
dateString := date.Format("Monday, January 2, 2006")
```

Note that we use the reference date of January 2, 2006 as our format, as it was chosen by the creators of Go as a placeholder. The output of `dateString` would be "Monday, September 13, 2021".

We can also use other date formatting options, such as the day of the week (Monday, Tuesday, etc.), the month (January, February, etc.), or the year (06, 2006, etc.). You can find a full list of formatting options in the [Go documentation](https://golang.org/pkg/time/#pkg-constants).

## Deep Dive

Behind the scenes, the `Format()` method uses the `time.Format()` function, which takes in the same formatting options and returns a string representation of a date. It also has the option to accept a `layout` parameter, which allows for more customization in the format of the date string.

Additionally, the `time` package in Go also provides other useful functions for converting a date into a string, such as `Parse()` for parsing a string into a date, `FormatDuration()` for converting a duration into a string, and more. For more in-depth information about working with dates and times in Go, check out the [official documentation](https://golang.org/pkg/time/).

## See Also

- [Official Go Documentation](https://golang.org/)
- [Go Time Package Documentation](https://golang.org/pkg/time/)
- [Date and Time Formats](https://www.w3schools.com/js/js_date_formats.asp)
- [Formatting Dates and Times in Go](https://www.educative.io/edpresso/formatting-dates-and-times-in-go)

By now, you should have a better understanding of how to convert a date into a string in Go. Whether you need to display a date or manipulate it in your program, the `time` package in Go provides a simple and efficient way to handle dates and times. Happy coding!