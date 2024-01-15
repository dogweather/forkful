---
title:                "Getting the current date"
html_title:           "Gleam recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Getting the current date is a common task in programming that can be useful in various applications. Whether it's for displaying the date on a website, creating a timestamp for log data, or even scheduling tasks, having accurate and efficient ways to get the current date is essential.

## How To

To get the current date in Gleam, we can use the `DateTime` module from the `gleam/datetime` package. This module provides functions for creating and manipulating dates and times. Let's take a look at some code examples:

```
Gleam import gleam/datetime

Gleam let today = DateTime.now()

Gleam io.print("Today's date is " ++ DateTime.Format.date(today))
// Output: Today's date is 2021-11-01
```

In the above example, we imported the `gleam/datetime` package and created a variable called `today` using the `DateTime.now()` function. We then used the `DateTime.Format.date()` function to format the date into a string that we printed to the console.

We can also get the current date in a specific time zone by passing the time zone as an argument to the `DateTime.now()` function. For example:

```
Gleam let pacific_time = DateTime.now(Time.Zone.more("-08:00"))
Gleam io.print("The current date in Pacific Time is " ++ DateTime.Format.date(pacific_time))
// Output: The current date in Pacific Time is 2021-11-01
```

In addition to getting the current date, we can also get the current time and date-time combination using the `DateTime.now.Time` and `DateTime.now.DateTime` functions, respectively.

## Deep Dive

Under the hood, the `DateTime.now()` function uses the operating system's clock to get the current date and time. This allows for accurate and reliable data, but it also means that the function's behavior may vary depending on the machine running the code.

Additionally, the `DateTime` module offers many other functions for manipulating dates and times, such as adding or subtracting days, months, and years, comparing dates, and converting between different time zones. It's worth exploring and familiarizing yourself with all the available functions for more advanced use cases.

## See Also

- Official Gleam Documentation on `DateTime` module: https://gleam.run/packages/gleam/datetime/latest/
- Gleam Standard Library on `DateTime` module: https://github.com/gleam-lang/gleam_stdlib/blob/main/gleam/datetime/src/datetime.gleam