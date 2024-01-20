---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is a common task where you analyze the difference between two points in time. Programmers often do this for tasks like scheduling, deadline tracking, or calculating durations.

## How to:

Let's dive right into it: the Go's time package is ideal for this, with its `After`, `Before`, and `Equal` methods. Here's a small code snippet demonstrating these:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t1 := time.Date(2022, 1, 1, 0, 0, 0, 0, time.UTC)
	t2 := time.Date(2022, 12, 31, 0, 0, 0, 0, time.UTC)

	if t1.Before(t2) {
		fmt.Println("t1 is before t2")
	}

	if t1.After(t2) {
		fmt.Println("t1 is after t2")
	}

	if t1.Equal(t2) {
		fmt.Println("t1 is the same as t2")
	}
}
```
This code will output: `t1 is before t2`, since in our example January 1st precedes December 31st of any given year.

## Deep Dive

Historically, date comparison has been a problematic area in computer programs due to slightly differing calendar systems worldwide and unexpected edge cases (like leap years). Go provides a solid solution with its time package, efficiently handling a variety of nuances. 

Alternatives to the methods `Before`, `After`, and `Equal` include subsetting the two times to get a Duration or directly comparing the Unix timestamp using the `Unix` method, but these might not warrant the same level of simplicity and readability. 

In terms of implementation, Go compares two dates utilizing Coordinated Universal Time (UTC), ensuring consistent results across different timezones. Note that date comparison in Go is exact, down to the nanosecond, in line with its precise time tracking.

## See Also

For additional information, check out the official documentation:
- [Go's time package](https://golang.org/pkg/time/)
- [Date and Time in Go](https://yourbasic.org/golang/dates-times-difference-duration/)
- [Go by Example: Time](https://gobyexample.com/time)