---
title:                "Rust recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Calculating dates in the future or past may seem like a simple task, but it is an important skill to have in programming. Many real-world applications, such as financial planning or scheduling, require handling dates and times. Being able to accurately calculate dates can ensure the smooth functioning of these applications.

## How To

To calculate a date in the future or past, we can use the `chrono` crate in Rust. First, we need to add the `chrono` crate to our dependencies in the `Cargo.toml` file. Then, we can use the `Local` struct to get the current date and perform calculations on it.

Here is an example of calculating a date 7 days from today:

```Rust
use chrono::{Local, Duration};

let today = Local::today();
let future_date = today + Duration::days(7);

println!("Today's date is: {}", today);
println!("7 days from now will be: {}", future_date);
```

The output of this code will be:

```
Today's date is: 2021-04-18
7 days from now will be: 2021-04-25
```

We can also calculate dates in the past by using negative values in our calculations. For example, if we want to find the date 2 weeks before today, we can use the following code:

```Rust
use chrono::{Local, Duration};

let today = Local::today();
let past_date = today - Duration::weeks(2);

println!("Today's date is: {}", today);
println!("2 weeks ago was: {}", past_date);
```

The output of this code will be:

```
Today's date is: 2021-04-18
2 weeks ago was: 2021-04-04
```

## Deep Dive

The `chrono` crate offers a range of methods for performing date and time calculations. We can use the `Duration` struct to add or subtract days, weeks, months, or years from a given date. Additionally, we can also use the `DateTime` struct to create custom dates and times.

It is important to note that the `chrono` crate uses the Gregorian calendar by default. This means that the calculations may not be accurate for dates before October 15, 1582, which is when the Gregorian calendar was adopted.

## See Also

- `chrono` crate documentation:
https://docs.rs/chrono/0.4.19/chrono/

- Date and Time handling in Rust:
https://www.rust-lang.org/learn/dates-and-time

- `DateTime` struct in Rust:
https://doc.rust-lang.org/std/time/struct.DateTime.html