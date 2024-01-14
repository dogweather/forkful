---
title:                "Gleam recipe: Calculating a date in the future or past"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past is a common task in programming. It allows for precise scheduling and time management, making it a useful skill for any developer.

## How To

To calculate a date in the future or past in Gleam, we will use the `Date` module. First, we need to import the module into our program:

```Gleam
import gleam/date
```

Next, we can use the `add_days` or `subtract_days` functions to add or subtract a specified number of days from a given date:

```Gleam
let date = date.from_gregorian(2020, 10, 1)
let future_date = date.add_days(10)
let past_date = date.subtract_days(5)
```

We can also use the `add_months` or `subtract_months` functions to add or subtract a specified number of months from a date, and the `add_years` or `subtract_years` functions to add or subtract a specified number of years.

For more precise control, we can use the `add_duration` and `subtract_duration` functions to add or subtract a specific duration, such as hours, minutes, or seconds, from a given date.

## Deep Dive

When calculating a date in the future or past, it's important to consider things like leap years and daylight saving time. In Gleam, the `Date` module handles these details for us, ensuring accurate calculations.

Additionally, the `from_gregorian` function used above also takes into account time zones, making it possible to calculate dates in a specific time zone if needed.

## See Also

- [Gleam documentation on date module](https://gleam.run/modules/date.html)
- [Examples of date calculations in Gleam](https://github.com/gleam-lang/examples/tree/master/date)
- [Gleam community forum](https://forum.gleam.run/)