---
title:                "Calculating a date in the future or past"
html_title:           "Gleam recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a date in the future or past is the process of obtaining a specific date by adding or subtracting a certain number of days, weeks, months, or years to or from a given date. Programmers often need to perform this task in order to create features such as scheduling, countdowns, and reminders in their applications.

## How to:

```
Gleam.DateTime.date_shift(
    date: DateTime,
    time_unit: DateTime.TimeUnit,
    value: Int
) -> DateTime
```

This function takes in a date, a time unit (day, week, month, or year), and a value to add or subtract. Let's say we want to find the date that is 5 days after January 1, 2022.

```
let date = DateTime.from_naive_date(2022, 1, 1)
let future_date = 
    DateTime.date_shift(date, DateTime.Day, 5)
```

The ```future_date``` will then be March 6, 2022. Similarly, if we want to find the date 2 months before October 15, 2020:

```
let date = DateTime.from_naive_date(2020, 10, 15)
let past_date = 
    DateTime.date_shift(date, DateTime.Month, -2)
```

The ```past_date``` will be August 15, 2020.

## Deep Dive

Calculating dates in the past or future has been made easier with the introduction of date and time libraries like Gleam's DateTime module. Before these libraries, programmers would have to manually calculate the date using arithmetic operations, which can be tedious and prone to error.

Other programming languages also have built-in functions for date shifting, such as Python's ```datetime.timedelta()``` and Ruby's ```Date.new()``` methods. However, Gleam's DateTime module is specifically designed for functional programming, making it easier and more efficient to use for functional programmers.

The implementation of the ```date_shift()``` function in Gleam is based on the concept of ```Duration```, which is the amount of time between two DateTime objects. This ensures that the function is accurate and consistent when calculating dates.

## See Also

- Gleam DateTime documentation: https://gleam.run/packages/gleam