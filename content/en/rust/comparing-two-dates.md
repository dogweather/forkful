---
title:                "Comparing two dates"
html_title:           "Rust recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered the task of comparing two dates in your Rust program? Maybe you need to check if a date is before or after another or calculate the number of days between them. Whatever the reason may be, knowing how to compare dates in Rust can be a useful skill for any developer.

## How To

It's actually quite simple to compare dates in Rust using the standard library's `DateTime` and `Duration` structs. Let's take a look at some coding examples to see how it's done.

First, we will need to import the `chrono` crate in our `Cargo.toml` file:

```Rust
[dependencies]
chrono = "0.4.19"
```

Next, we can use the `DateTime` struct to represent a specific date and time:

```Rust
use chrono::{DateTime, Utc};

let date_1: DateTime<Utc> = Utc::now();
let date_2: DateTime<Utc> = Utc::now();
```

Now, we can easily compare these dates using the following functions:

- `is_before`: returns `true` if `date_1` is before `date_2`
- `is_after`: returns `true` if `date_1` is after `date_2`
- `is_same`: returns `true` if `date_1` is the same as `date_2`

```
if date_1.is_before(date_2) {
    println!("Date 1 is before Date 2");
}
if date_1.is_after(date_2) {
    println!("Date 1 is after Date 2");
}
if date_1.is_same(date_2) {
    println!("Date 1 is the same as Date 2");
}
```

We can also use the `Duration` struct to calculate the difference between two dates in various units:

```
let duration = date_2 - date_1;
println!("The difference between the dates is {} days", duration.num_days());
```

## Deep Dive

Dates and times can be a complex topic, and there are a few things to keep in mind when comparing them in Rust. 

Firstly, it's important to note that we are using UTC (Coordinated Universal Time) in this example. This is a standard time zone used for global timekeeping and eliminates the need for time zones and daylight saving adjustments when comparing dates.

Secondly, when comparing dates with the `is_same` function, it is important to consider the precision of the `DateTime` struct. By default, it only has a precision of seconds, so dates with a difference of less than one second will be considered the same. You can increase the precision by using the `with_nanoseconds` function.

Lastly, when calculating the difference between dates with the `Duration` struct, it will automatically adjust for leap years and daylight saving time changes.

## See Also

- [Chrono Documentation](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust Standard Library Documentation on DateTime](https://doc.rust-lang.org/std/chrono/struct.DateTime.html)
- [Rust Standard Library Documentation on Duration](https://doc.rust-lang.org/std/time/struct.Duration.html)