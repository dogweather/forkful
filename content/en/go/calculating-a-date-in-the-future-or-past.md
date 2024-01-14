---
title:    "Go recipe: Calculating a date in the future or past"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past can be a useful tool for a variety of tasks, such as scheduling events, tracking deadlines, and conducting financial analyses. In Go programming, there are built-in functions and packages to help make these calculations easier and more accurate.

## How To

To calculate a new date based on a given date and a certain number of days in the future or past, we can use the `Date.Add()` function. Let's look at an example:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // create a variable with the current date
    current := time.Now()

    // add 7 days to the current date
    newDate := current.Add(7 * 24 * time.Hour)

    // print the new date in the desired format
    fmt.Println("New date after 7 days:", newDate.Format("January 2, 2006"))
}
```

Running this code will give us an output like this:

```markdown
New date after 7 days: September 16, 2021
```

In the above example, we used the `Date.Format()` function to specify the desired format of the date. You can choose from a variety of layout options, as listed in the official Go documentation.

If we want to calculate a date in the past, we can simply use a negative value in the `Date.Add()` function, like this:

```Go
time.Now().Add(-30 * 24 * time.Hour)
```

This will give us a date 30 days in the past.

## Deep Dive

Behind the scenes, the `Date.Add()` function is using the `Date.AddDate()` function to perform the calculations. This function takes in parameters for years, months, and days, and adds them to the given date. This allows for more flexibility in calculating dates, such as adding specific months or years in addition to days.

Furthermore, the `Time` package in Go also includes functions for comparing and manipulating dates, such as `Date.Before()` and `Date.After()`.

See Also
- [Go official documentation on time package](https://pkg.go.dev/time)
- [Tutorial on working with dates and times in Go](https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-and-times-in-go)