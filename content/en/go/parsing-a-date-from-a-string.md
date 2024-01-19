---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Let's Parse a Date in Go

## What & Why?
Parsing a date from a string means converting textual data into a `Date` object. Doing so enables programmers to manipulate dates, like calculating the number of days between two dates or sorting them.

## How to:
In Go, we use `time.Parse` to convert a string into a Time value. Check out this short snippet:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t, _ := time.Parse("2006-01-02", "2021-09-23")
	fmt.Println(t)
}
```
In console, you'd get:
```
2021-09-23 00:00:00 +0000 UTC
```
Note: In Go, the layout string used with time.Parse/Format must be "2006-01-02 15:04:05". It's a reference point, not a format pattern.

## Deep Dive
In the early days of Go (circa 2007), parsing and formatting time was a puzzle. The unique approach by Go's `time.Parse` and `Format` using a reference point date ("2006-01-02 15:04:05"), simplified things for programmers around the globe. 

An alternative way to parse a date in Go is by using the flexibility of `time.ParseInLocation`. It allows parsing a string into a `Time` considering a specific location, which helps deal with different time zones effectively.

Regarding implementation, `time.Parse` works by comparing the input string with the layout string, getting a match for the year, month, day, etc., and storing them in the Time object.

## See Also
- Go Time Package Documentation for more details: https://golang.org/pkg/time/
- An excellent tutorial on Golang Date and Time: https://yourbasic.org/golang/format-parse-string-time-date-example/ 
- More on Go time format: https://flaviocopes.com/golang-time-format/