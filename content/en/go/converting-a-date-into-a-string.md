---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a date into a string in any programming language, including Go, is a fundamental task that changes a Date object into a textual representation. Programmers do this to display dates in a human-readable format or to easily organize and manipulate date data.

## How to:

Here's how you can convert a date into a string in Go.

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    t := time.Now()
    fmt.Println(t.Format(time.RFC3339))  
}
```
Sample output:

`2021-12-26T00:51:40.123Z`

## Deep Dive:

Historically, date-time strings have varied based on local conditions. To create a worldwide standard, the Internet Engineering Task Force (IETF) published the RFC 3339 timestamp format. Go supports this format and several others.

There are alternative ways of handling dates, such as using third-party libraries, which offer functionalities like flexible formatting, timezone handling, and date arithmetic. 

When you convert a date into a string in Go, the `Format` function of the `time` package is used. By default, Go uses the UTC (Coordinated Universal Time) for date and time. If you want to specify a different time zone, you have to explicitly set it.

## See Also:

1. [Go’s time package documentation](https://pkg.go.dev/time)
2. [The RFC 3339 timestamp format](https://www.ietf.org/rfc/rfc3339.txt)
3. [Go’s time.Format layout reference](https://yourbasic.org/golang/format-parse-string-time-date-example/)