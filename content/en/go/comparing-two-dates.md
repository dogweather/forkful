---
title:                "Comparing two dates"
html_title:           "Go recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

One common task in programming is comparing two dates. This can be useful for tasks such as sorting data, checking for scheduling conflicts, or determining the amount of time between two events. In this article, we will explore how to compare dates using Go, the current version of the popular programming language.

## How To

To compare two dates in Go, we can use the `Equal()` and `Before()` methods from the `time` package. These methods take in two `time.Time` objects, which represent specific moments in time, and return `true` or `false` depending on the comparison result.

Let's look at an example of comparing two dates:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// creating two `time.Time` objects with different dates
	date1 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)
	date2 := time.Date(2021, time.February, 1, 0, 0, 0, 0, time.UTC)

	// using `Equal()` method to check if dates are equal
	equal := date1.Equal(date2)
	fmt.Println("Dates are equal:", equal)

	// using `Before()` method to check if date1 comes before date2
	before := date1.Before(date2)
	fmt.Println("Date1 comes before date2:", before)
}
```

The output of this code will be:
```
Dates are equal: false
Date1 comes before date2: true
```

In this example, we first create two `time.Time` objects with different dates. Then, we use the `Equal()` method to check if the dates are equal, which returns `false` since they are not the same date. Next, we use the `Before()` method to check if date1 comes before date2, which returns `true` since date1 is January 1st and date2 is February 1st. 

We can also use the `After()` method to check if date1 comes after date2, and the `Before()` and `After()` methods can also be used for `time.Date` objects that have different times as well.

## Deep Dive

Internally, the `Equal()` and `Before()` methods compare the underlying `int64` values of the two `time.Time` objects. These values represent the number of nanoseconds since January 1, 1970 UTC. This means that when comparing dates, the time component is not taken into consideration.

However, if we want to compare dates with the time component, we can use the `Before()` and `After()` methods on the `time.Time` objects after trimming them to the same time zone using the `Truncate()` method. For example:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// creating two `time.Time` objects with different dates and times
	date1 := time.Date(2021, time.January, 1, 12, 0, 0, 0, time.UTC)
	date2 := time.Date(2021, time.January, 1, 5, 0, 0, 0, time.UTC)

	// trimming both dates to UTC 
	date1 = date1.Truncate(time.Hour)
	date2 = date2.Truncate(time.Hour)

	// using `After()` method to check if date1 comes after date2
	after := date1.After(date2)
	fmt.Println("Date1 comes after date2:", after)
}
```

The output of this code will be:
```
Date1 comes after date2: true
```

In this example, we first create two `time.Time` objects with different dates and times. Then, we truncate both of them to the hour level using the `Truncate()` method, effectively removing the time component. This allows us to compare just the dates using the `After()` method, which returns `true` since date1 is after date2.

## See Also

- Go `time` package documentation: https://golang.org/pkg/time/
- Date and time formats in Go: https://yourbasic.org/golang/format-parse-string-time-date-example/