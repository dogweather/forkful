---
title:                "Calculating a date in the future or past"
date:                  2024-01-20T17:31:53.447751-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculating a date in the future or past"

category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a future or past date means figuring out what the calendar will say after or before a specified amount of time. Programmers do it for anything time-related, like setting reminders, expiration dates, or scheduling events.

## How to:

Rust has the `chrono` crate for all your date and time needs. Here's how to add to or subtract from a date:

```rust
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now = Utc::now();
    println!("Current UTC time: {}", now);

    let two_weeks = Duration::weeks(2);
    let future_date = now + two_weeks;
    println!("UTC in two weeks: {}", future_date);

    let thirty_days_ago = Duration::days(-30);
    let past_date = now + thirty_days_ago;
    println!("UTC 30 days ago: {}", past_date);
}
```

Sample output:

```
Current UTC time: 2023-04-01T12:00:00Z
UTC in two weeks: 2023-04-15T12:00:00Z
UTC 30 days ago: 2023-03-02T12:00:00Z
```

## Deep Dive

Traditionally, date and time manipulation has been a pain. Different systems and programming languages handle it in various ways. Rust's standard library provides basic functionality, but the `chrono` crate is the go-to.

Alternatives? Sure, you could manually calculate dates by converting everything to timestamps, manipulating the numbers, and converting back. Or, you could use time-specific libraries in other languages—Python has `datetime`, JavaScript has `Date`, and so on.

The `chrono` crate in Rust gives you time-zone aware types like `DateTime`, and durations as seen above. It handles all the messy bits like leap years and daylight savings so you don't have to. It also does date parsing and formatting, making it a comprehensive solution.

## See Also

- The `chrono` crate: https://crates.io/crates/chrono
- Rust's time documentation: https://doc.rust-lang.org/std/time/index.html
- Rust Date and Time chapter in "The Rust Programming Language" book: https://doc.rust-lang.org/book/ch10-02-traits.html (look for DateTime-related sections)
