---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:36:20.140382-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date means converting a string into a date object. Programmers do this to handle dates in a standardized form for storing, sorting, or manipulating.

## How to:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Example date string
	dateStr := "2023-04-01T15:04:05Z"

	// Define layout matching the above format
	layout := time.RFC3339

	// Parse string to time.Time object
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		panic(err)
	}
	
	// Output
	fmt.Printf("Parsed Date: %v\n", parsedDate)
}
```
Output:
```
Parsed Date: 2023-04-01 15:04:05 +0000 UTC
```

## Deep Dive
Date parsing has always been crucial for software systems to organize and process temporal information since, well, pretty much the dawn of computing. In Go, `time.Time` is the struct representing time. It's designed for simplicity and efficiency. Why use strings to start with? Mainly because dates come as text from different sources like APIs or user input. 

Alternatives? Well, you could theoretically do manual parsing, but it's prone to errors. Go's `time.Parse` function lets you define a layout – a reference date – which you compare your input against. It's a robust method because as of Go 1 (circa 2012), this psychological metric for human-readable time keeps your parsing on point. Python's `datetime` and Java's `SimpleDateFormat` offer similar functionality, but they aren't as strict as Go's parse implementation, which doesn't try to guess what you meant.

Here's the kicker: Go's parse function needs a specific reference time: `Mon Jan 2 15:04:05 MST 2006` (01/02 03:04:05PM '06 -0700). Remember this exact sequence; many people do by the mnemonic phrase, "1 2 3 4 5 6 7".

## See Also
- Go by Example: Time Formatting/Parsing: https://gobyexample.com/time-formatting-parsing
- Go time package doc: https://pkg.go.dev/time#Parse
- The Go Blog on Time: https://blog.golang.org/time
