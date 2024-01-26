---
title:                "Getting the current date"
date:                  2024-01-20T15:14:29.645601-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in Go involves grabbing the present moment's date from the system. Programmers track time to timestamp events, schedule tasks, or simply show users the current date.

## How to:

Here's how you get the current date in Go:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	fmt.Println("Current Date is:", currentTime.Format("2006-01-02"))
}
```

If you run this, you'd get output similar to:
```
Current Date is: 2023-04-05
```
The format `"2006-01-02"` might look odd, but in Go, this particular sequence is the reference layout for date formatting.

## Deep Dive

Before Go, programming languages each had their own quirky ways to manage dates and times. Go simplifies it, mostly. Why the numbers `2006-01-02 15:04:05`? They're a mnemonic—the 1st to 7th numbers in increasing order, mapping to `year-month-day hour:minute:second`.

There are alternatives like using Unix time (seconds since January 1, 1970) but it’s less human-readable. The `time` package in Go provides utilities for both formats and more, considering time zones and daylight saving. Under the hood, `time.Now()` communicates with your system's clock to fetch the current date and time.

## See Also

For more details on the Go `time` package, check the official docs:
- Go `time` package: https://pkg.go.dev/time

To dive into formatting and parsing dates in Go:
- Go by Example - Time Formatting / Parsing: https://gobyexample.com/time-formatting-parsing

Understanding time zones in Go programming:
- Working with Time Zones in Go: https://www.alexedwards.net/blog/working-with-time-zones
