---
title:                "Calculating a date in the future or past"
html_title:           "Go recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a date in the future or past is about determining the precise calendar date by adding or subtracting a given number of days, months, or years from a particular point in time. Programmers often do it for tasks like scheduling events, measuring time intervals, or setting deadlines.

## How To:

Here's an example of how to calculate a future or past date in Go using in-built time package:

To get a future date:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	today := time.Now()
	futureDate := today.AddDate(1, 0, 0) //Add 1 year

	fmt.Println("Today's Date: ", today)
	fmt.Println("Future Date: ", futureDate)
}
```
Output of the above script will be today's date and a date a year from now.

And, here's how you get a past date:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
    today := time.Now()
    pastDate := today.AddDate(-1, 0, 0) //Subtract 1 year

    fmt.Println("Today's Date: ", today)
    fmt.Println("Past Date: ", pastDate)
}
```
This will output today's date and the same date last year.

## Deep Dive

Historically, calculating dates in programming languages came with the advent of time libraries. Before that, countless bugs had occurred due to mishandling date and time calculation. In Go, the 'time' package is used for date and time operations.

An alternative way of calculating future or past dates in Go is by using the 'Add' method provided in the 'time' package. This method adds the duration provided as a parameter to the current time. However, 'AddDate' provides better management by allowing you to individually increment or decrement years, months, or days.

When calculating future or past dates, it is crucial to be mindful of leap years. Luckily, Go's 'time' package handles this complexity for you.

## See Also

1. Go's official time package documentation: https://golang.org/pkg/time/
2. An introduction to Go's time package: https://yourbasic.org/golang/time-date-difference-timestamp/
3. Converting times between time zones in Go: https://blog.golang.org/here-is-the-time