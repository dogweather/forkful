---
title:                "Comparing two dates"
html_title:           "Elm recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is a way of determining which date comes first chronologically and allows programs to perform tasks like sorting events chronologically. This is essential in a variety of applications like scheduling systems, navigation apps, and timeline-based algorithms.

## How to:

In Rust, the `chrono` crate provides easy-to-use functionalities for date-time manipulation, including comparison. You can add it to your `Cargo.toml`:

```Rust
[dependencies]
chrono = "0.4.19"
```

Then, you can compare dates as:
```Rust
use chrono::{NaiveDate, Date, Utc};

fn main() {
    let date1 = NaiveDate::from_ymd(2022, 7, 10);
    let date2 = NaiveDate::from_ymd(2023, 7, 10);

    if date1 > date2 { 
        println!("date1 is recent"); 
    } else { 
        println!("date2 is recent"); 
    }
}
```

Output would be:
```
date2 is recent
```

## Deep Dive

Historically, managing date and time functions was no simple venture for programmers. Early languages did not even have an inherent type for the date-time structure. 

There are other date-time libraries for Rust aside from `chrono` like `time` and `date-time`, but `chrono` offers a sound balance between simplicity and feature-richness.

As for implementation details, it's noteworthy that `chrono` deals with date-time values as immutable, ensuring correctness and consistency. Generally, the comparison operation is done by converting dates to a common format (like Unix timestamp), and then these era-clear values get compared.

## See Also

- [Chrono Documentation](https://docs.rs/chrono/0.4.19/chrono/index.html)
- [Rust Time Library](https://docs.rs/time/0.3.4/time/index.html)