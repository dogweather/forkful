---
title:                "Getting the current date"
html_title:           "Go recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Getting the current date means retrieving the current date and time on your device. Programmers often do this to track when a program was run, for time-based operations, or for user interface purposes.

## How to:
To get the current date in Go, you can use the `time` package. Here's an example of fetching and displaying the current date and time in the `YYYY-MM-DD hh:mm:ss` format:

```Go
package main

import (
  "fmt"
  "time"
)

func main() {
  currentDate := time.Now().Format("2006-01-02 15:04:05")
  fmt.Println("The current date and time is:", currentDate)
}
```

Output:
```
The current date and time is: 2021-04-07 21:14:32
```

## Deep Dive:
1. Historical Context: Before the release of Go 1.9 in 2017, the `time` package did not have a `Format()` method, making it slightly more cumbersome to get the current date and time. However, the addition of this method has made it much easier and streamlined.
2. Alternatives: Another way to get the current date and time in Go is by using the `time.Now()` function without formatting it. This will give you the date and time in the format `time.Time` instead of a string.
3. Implementation Details: The `time` package in Go uses a `Time` struct to represent dates and times. It stores the date and time in UTC format by default, but you can specify a specific time zone when retrieving the current date and time.

## See Also:
- [The `time` package documentation](https://golang.org/pkg/time/)
- [A tutorial on working with dates and times in Go](https://golang.org/doc/tutorial/time/)