---
title:                "Parsing a date from a string"
html_title:           "Go recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 
"Parsing a date from a string" is the process of converting a date or time string into a date or time format that can be understood and manipulated by a computer program. Programmers often need to parse date strings in order to perform operations such as sorting, filtering, and comparing dates in their programs.

## How to: 
Here is a simple example of how to parse a date from a string using the standard library "time" package in Go:
```Go
dateString := "July 4, 2021"
layout := "January 2, 2006"
date, err := time.Parse(layout, dateString)
if err != nil {
  fmt.Println("Error parsing date:", err)
} else {
  fmt.Println(date)
}
```
Output:
```Go
2021-07-04 00:00:00 +0000 UTC
```

## Deep Dive: 
Parsing dates from strings has been around since the early days of computer programming, but it has become more important as applications have become more complex. The "time" package in Go provides easy-to-use functions for parsing different date and time formats, making it a popular choice for developers. Other alternatives for parsing dates in Go include using regular expressions or external libraries. Internally, the "time" package uses the Unix time format, which represents time as a number of seconds since January 1, 1970.

## See Also: 
- Go "time" package documentation: https://pkg.go.dev/time
- Regular expressions in Go: https://pkg.go.dev/regexp
- "moment" library for parsing dates in Go: https://github.com/moment/moment