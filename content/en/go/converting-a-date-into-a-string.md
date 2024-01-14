---
title:    "Go recipe: Converting a date into a string"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

When working with dates and time in a Go program, there may be instances where you need to convert a date into a string. This could be for displaying the date in a specific format, or for storing the date in a database. In this blog post, we will explore how to convert a date into a string in Go.

## How To

Converting a date into a string in Go is a relatively straightforward process. There are several ways to do it, depending on your specific requirements. Let's take a look at some code examples to better understand the process.

First, let's define a variable with a date value:

```Go
date := time.Date(2021, time.March, 20, 0, 0, 0, 0, time.UTC)
```

### Format the Date

To convert this date into a string, we can use the `Format()` method from the `time` package. This method takes a layout string as the argument, which specifies how the date should be formatted.

```Go
dateString := date.Format("02-01-2006")
fmt.Println(dateString) // Output: 20-03-2021
```

In this example, we used the layout "02-01-2006" to format the date into DD-MM-YYYY format. This might be useful for displaying the date in a more human-readable format.

### Convert to Unix Timestamp

Another way to convert a date into a string is by using the `Unix()` method. This method returns the date in seconds since January 1, 1970 UTC.

```Go
unixDateString := strconv.FormatInt(date.Unix(), 10)
fmt.Println(unixDateString) //Output: 1616208000
```

If you need to store the date in a database or perform calculations with it, using a Unix timestamp might be a better approach.

## Deep Dive

Go's `time` package offers a wide range of methods for manipulating and formatting dates. You can specify the layout string in various formats, depending on your needs. Additionally, Go also supports the parsing of dates from strings using the `Parse()` method.

## See Also

Here are some related links that might be helpful in further understanding date and time operations in Go:

- [Go Time Package Documentation](https://golang.org/pkg/time/)
- [Converting Time to Strings in Go](https://www.calhoun.io/converting-time-to-strings-in-go/)
- [Parsing Dates and Times in Go](https://blog.gopheracademy.com/advent-2014/parsing-dates-and-times-in-go/)