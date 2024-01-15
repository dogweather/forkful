---
title:                "Comparing two dates"
html_title:           "Gleam recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why
If you've ever needed to compare two dates in your code, you know it can be a tricky task. Whether you're working on a financial application or just trying to schedule a meeting, properly comparing dates is essential. In this article, we'll explore how Gleam can make this process easier and more efficient.

## How To
To compare two dates in Gleam, we will use the built-in Date module. First, we need to create two date records using the `Date.from_fields` function. This function takes in the year, month, and day as arguments and returns a `Date` record. For example:

```Gleam
let date1 = Date.from_fields(2020, April, 15)
let date2 = Date.from_fields(2020, May, 20)
```

Next, we can use the `Date.compare` function to compare the two dates. This function takes in two `Date` records and returns an `Ordering` record. `Ordering` is an enum with three possible values - `Less`, `Equal`, and `Greater` - indicating whether the first date is less than, equal to, or greater than the second date. For example:

```Gleam
let ordering = Date.compare(date1, date2)

// Output: Greater
```

Based on the output, we can determine that `date1` is later than `date2`.

## Deep Dive
While the `Date.compare` function is perfect for simple comparisons, there are times when we may need more detailed information about the two dates. In these cases, we can use the `Date.diff` function. This function takes in two `Date` records and returns a `Date.Diff` record. The `Date.Diff` record contains the difference between the two dates in years, months, and days. For example:

```Gleam
let date_diff = Date.diff(date1, date2)

// Output: 0 years, 1 month, 5 days
```

This tells us that `date2` is 1 month and 5 days after `date1`.

Another useful function for comparing dates is `Date.is_before`. This function takes in two `Date` records and returns a boolean value indicating whether the first date is before the second date. This can be helpful for filtering data or scheduling tasks based on date criteria.

## See Also
- Gleam Date module: https://gleam.run/modules/date
- Gleam standard library: https://gleam.run/documentation/stdlib.html
- How to compare two dates in JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date