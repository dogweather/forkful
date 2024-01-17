---
title:                "Converting a date into a string"
html_title:           "Go recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a date into a string allows programmers to display the date in a human-readable format, making it easier to understand and work with. This is commonly needed for tasks such as displaying dates on a user interface or saving dates in a database.

## How to:
Converting a date into a string in Go is easy and can be done using the `format` function from the `time` package. Below is a code snippet showing how to do this and its corresponding output:

```Go
package main

import (
  "fmt"
  "time"
)

func main() {
  // Create a date object using the current time
  date := time.Now()

  // Convert the date into a string with the desired format
  dateString := date.Format("Mon, Jan 02, 2006")

  // Print the resulting string
  fmt.Println(dateString)
}
```
Output:
```
Mon, Oct 18, 2021
```

The `Format` function takes in a layout string which specifies how the date should be formatted. The format uses a reference time "Mon Jan 2 15:04:05 -0700 MST 2006" to determine the formatting. You can mix and match different values to customize the layout to your preference.

## Deep Dive:
Converting a date into a string has been a common task for programming languages since the early days of computing. In fact, even before computers, people have been using different methods to represent dates in a more understandable and consistent way. In Go, the `Format` function is the standard way to convert a date into a string, but there are also alternatives such as the `String` method.

Under the hood, the `Format` function uses a combination of format rules and the reference time to generate the resulting string. This function also takes into account the timezone and any specific location information for more accurate formatting.

## See Also:
- Official documentation for the `time` package in Go: https://pkg.go.dev/time
- The Go tour interactive tutorial on the `time` package: https://tour.golang.org/basics/15
- A detailed explanation of all the layout values for the `Format` function: https://programming.guide/go/format-parse-string-time-date-example.html