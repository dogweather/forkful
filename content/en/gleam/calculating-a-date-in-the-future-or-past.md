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

## Why

Do you ever need to calculate a future or past date in your code? Maybe you're creating a scheduling system or need to determine a deadline for a project. Whatever the reason, Gleam has got you covered with its built-in date calculation functions.

## How To

Calculating a date in the future or past in Gleam is as simple as using the built-in `Date` module and its functions. Let's take a look at an example of calculating a date 30 days in the future from today:

```Gleam
import Date

let today = Date.now()
let thirty_days_later = Date.add_days(today, 30)

// Output: 30 days from today is: 2021-08-07T11:15:34.826Z
```

We first import the `Date` module and use the `now()` function to get the current date and time. Then, we use `add_days()` to add 30 days to that date, giving us the date 30 days in the future. Gleam also has other functions for calculating dates in the future or past, such as `add_months()` and `add_years()`, which work in a similar way.

## Deep Dive

Behind the scenes, Gleam uses the Erlang standard library `calendar` module to handle date and time calculations. This module provides accurate and efficient functions for handling date and time values, taking into account things like leap years and timezones. By using the `Date` module in Gleam, you can be sure that your date calculations will be accurate and reliable.

## See Also

- [Gleam Date Module Documentation](https://gleam.run/modules/datum/date/)
- [Erlang Calendar Module Documentation](https://erlang.org/doc/man/calendar.html)