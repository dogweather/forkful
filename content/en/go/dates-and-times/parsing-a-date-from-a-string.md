---
date: 2024-02-03 17:50:07.310824-07:00
description: "How to: Go provides robust support for parsing dates and times through\
  \ the `time` package. The key is understanding Go's reference date format: `Mon\
  \ Jan 2\u2026"
lastmod: '2024-03-13T22:44:59.639504-06:00'
model: gpt-4-0125-preview
summary: Go provides robust support for parsing dates and times through the `time`
  package.
title: Parsing a date from a string
weight: 30
---

## How to:
Go provides robust support for parsing dates and times through the `time` package. The key is understanding Go's reference date format: `Mon Jan 2 15:04:05 MST 2006`, which you use to tell Go how to interpret the incoming string. Here’s a quick example to get you started:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Example date string
	dateStr := "2023-04-12 14:45:00"
	
	// Define the layout/format of the input date string
	// This layout tells Go to expect a year, followed by a month, 
	// then a day, hour, minute, and finally second
	layout := "2006-01-02 15:04:05"
	
	// Parse the date string according to the layout
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Error parsing date:", err)
		return
	}
	
	// Output the parsed date
	fmt.Println("Parsed Date:", parsedDate)
}
```

When you run this code, you’ll get:

```
Parsed Date: 2023-04-12 14:45:00 +0000 UTC
```

Note how the `layout` string uses the reference date's values to specify the format of the input string. Adjust the `layout` to match the format of your input dates.

## Deep Dive
The design of Go's date and time parsing is unique, utilizing a specific reference date (`Mon Jan 2 15:04:05 MST 2006`). This approach, instead of using more conventional format specifiers (like `YYYY` for year), was chosen for readability and ease of use, leveraging a more example-based format. 

While this can initially seem unusual to programmers accustomed to other languages, many find it more intuitive after a brief adjustment period. For applications requiring more complex date manipulation or formats not directly supported by Go's `time` package, third-party libraries such as `github.com/jinzhu/now` can offer additional functionality. However, for the majority of standard applications, Go's built-in capabilities are robust, performant, and idiomatic, embodying the Go philosophy of simplicity and clarity.
