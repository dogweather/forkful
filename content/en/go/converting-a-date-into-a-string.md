---
title:                "Converting a date into a string"
date:                  2024-01-20T17:36:42.521261-07:00
model:                 gpt-4-1106-preview
simple_title:         "Converting a date into a string"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a date to a string means changing a date's format from one used by the computer to one humans can easily read. Programmers do this to display dates on interfaces or to format them for reports and logs.

## How to:
In Go, converting a date to a string is pretty straightforward with the `time` package.

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now()
	fmt.Println("Formatted Date:", currentTime.Format("2006-01-02 15:04:05"))
}
```

Output might look something like this:
```
Formatted Date: 2023-04-07 14:21:34
```

The `Format` method uses a special reference date: Mon Jan 2 15:04:05 MST 2006. You match your desired format to the layout of this reference date. Neat trick, huh?

## Deep Dive
Go's `time` package handles date and time operations. The `Format` method from the `time.Time` struct is a workhorse. 

Why the odd reference date "2006-01-02 15:04:05"? In Go's early days, this pattern was chosen because the numbers (1 through 7) are each unique and increment by 1, so each represents a different component of the time format. This makes it quirky but intuitive once you get it.

Alternatives? Sure, we've got third-party libraries like `timeparse` or `strftime` that mimic other language's time handling. But for most of us, the standard library does the job just fine.

Behind the scenes, formatting involves parsing the reference time layout and replacing parts with corresponding values from the actual time being formatted. It also handles timezone conversions – a must for global apps.

## See Also
For a deep dive into the Go `time` package, check out: 
- The official docs at https://pkg.go.dev/time
- Go by Example's take on date formatting: https://gobyexample.com/time-formatting-parsing

When stack overflow hits, these threads can be a lifesaver:
- Time Formatting: https://stackoverflow.com/questions/20234104/how-to-format-current-time-using-a-yyyymmddhhmmss-format
- Parsing Strings into Time: https://stackoverflow.com/questions/14106541/how-to-parse-date-string-in-go