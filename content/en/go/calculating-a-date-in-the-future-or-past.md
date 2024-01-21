---
title:                "Calculating a date in the future or past"
date:                  2024-01-20T17:31:12.352599-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculating a date in the future or past"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a date in the future or past is just what it sounds like - figuring out what date it'll be, say, 10 days from now, or what date it was 50 days ago. Programmers do this for stuff like setting deadlines, expiry dates, or handling reservations.

## How to:

Let's mess with time in Go:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Today's date
	today := time.Now()
	fmt.Println("Today is:", today.Format("Jan 2, 2006"))

	// Future date, 10 days from now
	future := today.Add(10 * 24 * time.Hour)
	fmt.Println("10 days from now:", future.Format("Jan 2, 2006"))

	// Past date, 50 days ago
	past := today.Add(-50 * 24 * time.Hour)
	fmt.Println("50 days ago:", past.Format("Jan 2, 2006"))
}
```

Run it and you'll see something like:

```
Today is: Mar 15, 2023
10 days from now: Mar 25, 2023
50 days ago: Jan 24, 2023
```

## Deep Dive

Why care about dates? Well, historically, tracking time has been key for agriculture, science, history, you name it. In computing, it's just as crucial - think of tasks like backups, expiration checks, and scheduling.

Before Go's `time` package, we had to rely on less intuitive libraries or, heaven forbid, manual calculations. Now, we can manipulate dates using `Add` for adding durations, or `Sub` for finding the duration between two dates.

Also, here's a fun fact: calculations consider leap years and stuff, but there's no handling for quirks in human-made calendars (like when Britain skipped 11 days in 1752).

Alternatives? Sure. You could use `AddDate` to add specific numbers of years, months, and days, if you don't fancy the `duration * time.Hour` approach.

Implementation-wise, Go uses a proleptic Gregorian calendar, extended back to year one and forward to the far future. It's the same system we use daily, minus quirks of historical calendar reforms.

## See Also

- The Go Programming Language Specification on time: https://golang.org/ref/spec#Time
- The Go `time` package docs: https://pkg.go.dev/time
- Rob Pike’s talk on Go’s Time Formatting: https://www.youtube.com/watch?v=rKnDgT73v8s