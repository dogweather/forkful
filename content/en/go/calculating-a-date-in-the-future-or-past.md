---
title:    "Go recipe: Calculating a date in the future or past"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past is a common task in programming, especially when dealing with scheduling or planning applications. With Go's built-in time package, it becomes a simple and straightforward process. In this blog post, we will explore how to calculate dates in the past or future using Go.

## How To

To calculate a date in the future or past, we first need to understand how dates are represented in Go. Go's time package uses the type `time.Time` which represents a point in time. Let's take a look at an example of how we can use this to calculate a date in the future.

```Go
package main

import (
  	"fmt"
  	"time"
)

func main() {
  	// Get the current date and time
  	now := time.Now()

  	// Add 7 days to the current date
  	future := now.AddDate(0, 0, 7)

  	// Format the date output as "Month Day, Year"
  	fmt.Println(future.Format("January 2, 2006"))
}
```

The output of the above code will be the date 7 days from now, in the format "Month Day, Year". We can also calculate dates in the past by using a negative number in the `AddDate` function.

```Go
// Subtract 2 months from the current date
past := now.AddDate(0, -2, 0)
```

In addition to adding or subtracting days, months and years, we can also use the `Date` function to set a specific date.

```Go
// Set a specific date: January 1st, 2022
specificDate := time.Date(2022, time.January, 1, 0, 0, 0, 0, time.UTC)
```

Now that we know how to calculate dates in the future or past, let's dive deeper into the time package and explore some of its other useful functions.

## Deep Dive

In addition to `AddDate` and `Date`, Go's time package offers other functions such as `Sub`, which calculates the difference between two dates. We can also use the `Before` and `After` functions to compare two dates.

Go's time package also allows us to format dates in different ways using the `Format` function. We can specify the layout we want our date to be displayed in by using a specific date and time format reference.

```Go
// Format output as "Month/Day/Year Hour:Minute:Second"
fmt.Println(now.Format("01/02/2006 15:04:05"))
```

Additionally, Go's time package offers functions to extract specific information from a date, such as the day of the week or the time zone.

## See Also

- Go Time Package Documentation: https://golang.org/pkg/time/
- Beginner's Guide to Go: Working with Time and Dates: https://www.calhoun.io/working-with-dates-and-times-in-go/
- Manipulating Dates and Times in Go: https://medium.com/@felipedutratine/manipulating-dates-and-times-in-golang-bb2211b072e1