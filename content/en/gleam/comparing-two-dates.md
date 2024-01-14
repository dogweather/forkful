---
title:                "Gleam recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
Comparing dates is a common task in programming and can be useful in a variety of situations. Whether you need to check for overlapping events, calculate the time difference between two dates, or simply sort a list of dates, having the ability to compare dates is an essential skill for any programmer. In this post, we'll explore how to compare two dates in Gleam and the different ways you can use this feature in your code.

## How To
To start, let's define two dates using the `Date.from_calendar` function. This function takes in a record with the keys `year`, `month`, `day`, `hour`, `minute`, and `second` to create a `Date` type.

```
Gleam
let date1 = Date.from_calendar(
  year: 2021,
  month: 4,
  day: 15,
  hour: 12,
  minute: 30,
  second: 0
)

let date2 = Date.from_calendar(
  year: 2021,
  month: 4,
  day: 20,
  hour: 15,
  minute: 0,
  second: 0
)
```

Now that we have our two dates, we can use the `Date.compare` function to compare them. This function takes in two `Date` values and returns an `Ordering` type, which can be either `Less`, `Equal`, or `Greater`.

```
Gleam
let result = Date.compare(date1, date2)
```

To use this result in our code, we can use a `case` expression to handle the different scenarios.

```
Gleam
case result {
  Equal -> "The dates are the same"
  Less -> "Date1 is earlier than Date2"
  Greater -> "Date1 is later than Date2"
}
```

We can also use the `Date.between` function to check if a given date falls between two other dates. This function takes in a `Date` value and two other `Date` values and returns a `Bool` indicating whether the first date falls between the other two.

```
Gleam
let date3 = Date.from_calendar(
  year: 2021,
  month: 4,
  day: 17,
  hour: 9,
  minute: 0,
  second: 0
)

let is_between = Date.between(date3, date1, date2)
```

## Deep Dive
Behind the scenes, the `Date` type in Gleam is implemented using Erlang's `calendar` module, which is based on the Gregorian calendar. This module provides a high level of accuracy for working with dates, ensuring that Leap Years and Daylight Saving Time are taken into account. This means that you can rely on the `Date` type to accurately compare dates and handle edge cases.

It's also worth noting that the `Date` type does not include any time zone information. If you need to work with time zones, it's recommended to convert your dates to UTC before performing any comparisons.

## See Also
- Gleam documentation on `Date` type: https://gleam.run/documentation/std-lib/date.html
- Erlang `calendar` module: http://erlang.org/doc/man/calendar.html
- "Date and Time in Gleam" blog post by Connie Kennedy: https://conniekennedy.dev/writing/date-and-time-in-gleam/