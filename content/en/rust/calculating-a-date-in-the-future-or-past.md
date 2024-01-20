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

Calculating a date in the future or past means manipulating a base date, either adding or subtracting days, months, etc., to reach a new date. Programmers often do this for scheduling tasks, analyzing time-series data, or creating event-related functions.

## How To:

Let's use the Chrono library in Rust for handling these dates manipulations tasks.

First, you'll need to add Chrono to your dependencies in `Cargo.toml`:

```Rust
[dependencies]
chrono = "0.4"
```

Here's an example on how to add seven days to the current date:

```Rust
extern crate chrono;

use chrono::offset::Local;
use chrono::Duration;

fn main() {
    let now = Local::now();
    let in_a_week = now + Duration::days(7);
    println!("{}", in_a_week);
}
```

When you run this, it would output a date seven days from the time you executed it, formatted like this: "YYYY-MM-DD HH:MM:SS.ssssss +/-HHMM".

Similarly, you can subtract days from a date:

```Rust
extern crate chrono;

use chrono::offset::Local;
use chrono::Duration;

fn main() {
    let now = Local::now();
    let seven_days_ago = now - Duration::days(7);
    println!("{}", seven_days_ago);
}
```

The output will be a date seven days in the past from the time you executed it.

## Deep Dive

Historically, date calculations were no cakewalk due to the complex nature of calendar systems. These complexities birthed useful libraries like Chrono in Rust. 

If Chrono doesn't fit your needs, other alternatives include the time crate and date-time crate in Rust. However, Chrono offers a plethora of features and a user-friendly interface making it a popular choice among Rustaceans.

Delving deeper into implementation, Chrono uses the Olson database (also known as tz database) for time zones, letting us adjust date and time based on locations across the globe. This helps when you're dealing with users in different timezones.

## See Also

- Official Documentation for Chrono: https://docs.rs/chrono/0.4.19/chrono/
- The Rust Programming Language: https://doc.rust-lang.org/book/
- Rust Date-Time documentations: https://docs.rs/date-time/0.1.1/date_time/
- Rust time crate: https://docs.rs/time/0.1.43/time/