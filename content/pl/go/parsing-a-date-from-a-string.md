---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:36:31.028729-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Parsing dates converts string representations of dates and times into a structured format we can manipulate. Programmers do this to handle user input, store or display dates in different formats, and perform operations like date arithmetic.

## How to: (Jak to zrobić:)
Start by importing the `time` package. Use `time.Parse` to convert your string into a `time.Time` object. Here's a basic example:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	strDate := "2023-03-15 14:45:00"
	layout := "2006-01-02 15:04:05" // Go’s reference date
	parsedDate, err := time.Parse(layout, strDate)
	if err != nil {
		fmt.Println("Error parsing date:", err)
		return
	}
	fmt.Println("Parsed date:", parsedDate)
}
```

Sample Output:
```
Parsed date: 2023-03-15 14:45:00 +0000 UTC
```

## Deep Dive (Dogłębna analiza):
Date parsing in Go is unique. It uses a reference date, `Mon Jan 2 15:04:05 MST 2006`, to dictate the format. Why 2006? It's arbitrary, but the numbers (1 2 3 4 5 6 7) give each date and time component a distinct value, making it mnemonic.

Alternatives to `time.Parse` include third-party libraries that offer additional functionality or different interfaces. `time.ParseInLocation` is used to parse a date with a specific timezone.

Implementation-wise, Go parses strings based on patterns, not specific formats like other languages do. Once parsed, `time.Time` methods allow manipulation of dates, such as adding or subtracting time, comparison, and formatting to strings.

## See Also (Zobacz również):
- Go's time package documentation: https://golang.org/pkg/time/
- Go by Example – Time formatting and parsing: https://gobyexample.com/time-formatting-parsing
- The Go Blog on time and dates: https://blog.golang.org/time