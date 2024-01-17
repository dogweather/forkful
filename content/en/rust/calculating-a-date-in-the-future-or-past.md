---
title:                "Calculating a date in the future or past"
html_title:           "Rust recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a date in the future or past refers to the process of determining a specific date that is ahead or behind a given date. This is a common task in programming, especially for applications that require managing time-sensitive data or events.

## How to:

To calculate a date in the future or past using Rust, we can use the `chrono` crate. In the code blocks below, we will demonstrate how to add or subtract a certain number of days, months, or years from a given date.

### Adding days to a date:

```
use chrono::{NaiveDate, Duration};

let date = NaiveDate::from_ymd(2021, 10, 1);
let days_to_add = Duration::days(10);

let result_date = date + days_to_add;

println!("{}", result_date); // Output: 2021-10-11
```

### Subtracting months from a date:

```
use chrono::{NaiveDate, Datelike};

let date = NaiveDate::from_ymd(2021, 12, 1);
let months_to_subtract = 6;

let result_date = date.with_month(date.month() - months_to_subtract);

println!("{}", result_date); // Output: 2021-06-01
```

### Adding years to a date:

```
use chrono::{NaiveDate, Datelike};

let date = NaiveDate::from_ymd(2021, 7, 15);
let years_to_add = 3;

let result_date = date.with_year(date.year() + years_to_add);

println!("{}", result_date); // Output: 2024-07-15
```

## Deep Dive:

Calculating dates has been an important feature in programming since the early days of computing. In the past, this task was done manually by programmers using complex algorithms. Now, with the help of libraries like `chrono`, it has become much easier and more efficient to handle different date operations.

## See Also:

- `chrono` crate documentation: https://docs.rs/chrono/0.4.19/chrono/
- Comparison of date and time libraries in Rust: https://runebook.dev/en/docs/rust/date-time/chrono
- Date and Time in Rust (video tutorial): https://youtu.be/YujnB-xLpGw