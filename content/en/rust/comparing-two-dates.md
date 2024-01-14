---
title:    "Rust recipe: Comparing two dates"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

When working with dates in programming, it is common to need to compare two dates. This can be useful for tasks such as finding the difference between two dates or determining if a certain date falls within a range. In this blog post, we will discuss how to compare two dates in Rust and the different methods available for doing so.

## How To

To compare two dates in Rust, we first need to create `DateTime` objects using the `chrono` crate. Let's take a look at a simple example:

```Rust
use chrono::{DateTime, Utc};
use chrono::offset::TimeZone;

let dt1 = Utc::now(); // current date and time
let dt2 = Utc.ymd(2021, 10, 31).and_hms(12, 0, 0); // October 31st, 2021 at 12:00:00 PM

if dt1 > dt2 {
    println!("dt1 is after dt2");
} else if dt1 < dt2 {
    println!("dt1 is before dt2");
} else {
    println!("dt1 is the same as dt2");
}
```

In the example above, we first import the necessary types from the `chrono` crate. Then, we create two `DateTime` objects: `dt1` which is the current date and time using `Utc::now()`, and `dt2` which is a specific date and time using `Utc.ymd()` and `Utc.hms()`. Finally, we use conditional statements to compare the two dates and print out the result.

We can also compare dates using specific methods like `is_equal()`, `is_before()` and `is_after()`. Let's take a look at another example:

```Rust
use chrono::{DateTime, Utc};
use chrono::offset::TimeZone;

let dt1 = Utc::now(); // current date and time
let dt2 = Utc.ymd(2021, 10, 31).and_hms(12, 0, 0); // October 31st, 2021 at 12:00:00 PM

if dt1.is_equal(&dt2) {
    println!("dt1 is the same as dt2");
}

if dt1.is_before(&dt2) {
    println!("dt1 is before dt2");
}

if dt2.is_after(&dt1) {
    println!("dt2 is after dt1");
}
```

In this example, we call different methods on the date objects to compare them and print out the result.

## Deep Dive

The `DateTime` objects in Rust use UTC as the default time zone. However, we can also convert them to different time zones using the `with_timezone()` method. This can be helpful when comparing dates between different time zones.

Additionally, we can also compare dates using other date formats like `NaiveDateTime` and `Date`. `NaiveDateTime` does not contain time zone information and is useful for scenarios when we only need to compare dates without considering time zones. `Date` only contains date information and can be useful for tasks such as finding the difference between two dates without considering time.

## See Also

- Official `chrono` crate documentation for [DateTime](https://docs.rs/chrono/0.4.19/chrono/struct.DateTime.html)
- [Rust DateTime Cheat Sheet](https://devhints.io/datetime#rust)
- [Comparing Datetimes in Rust](https://www.tutorialspoint.com/comparing-datetimes-in-rust)