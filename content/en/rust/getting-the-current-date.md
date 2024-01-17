---
title:                "Getting the current date"
html_title:           "Rust recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date is a common task in programming, as it allows developers to keep track of time-related events and manage data based on dates. With the current date, programmers can calculate time intervals, schedule tasks, and organize data according to current events. 

## How to:

To get the current date in Rust, you can use the `chrono` crate, which provides a DateTime struct with the current date and time information. First, add the crate to your project's `Cargo.toml` file:
```
[dependencies]
chrono = "0.4.19"
```
Then, use the `Utc::now()` function to get the current date and time in the UTC timezone:
```
use chrono::{DateTime, Utc};

fn main() {
    let current_date: DateTime<Utc> = Utc::now();
    println!("{}", current_date);
}
```
This will print the current date and time in the format `YYYY-MM-DD HH:MM:SS UTC`, depending on your local time zone.

You can also get the current date and time in a specific time zone by using the `FixedOffset` struct and specifying the desired timezone offset in seconds:
```
use chrono::{DateTime, FixedOffset};

fn main() {
    let current_date: DateTime<FixedOffset> = FixedOffset::east(3600).from_local_datetime(&Local::now());
    println!("{}", current_date);
}
```

## Deep Dive:

Historically, getting the current date was a complex and unreliable process, as it was dependent on the system's hardware clock. With the introduction of NTP (Network Time Protocol), it became easier to get accurate and synchronized time from remote servers.

Alternative ways of getting the current date in Rust include the `time` crate, which provides a `time::OffsetDateTime` struct, and using system calls directly. However, using the `chrono` crate is currently the preferred method due to its easy-to-use API and cross-platform compatibility.

The implementation details of getting the current date in Rust can vary depending on the underlying operating system. On Windows, it uses the `winapi` crate to make direct system calls, while on Unix-based systems, it uses the `libc` crate.

## See Also:

- [chrono crate documentation](https://docs.rs/chrono/)
- [time crate documentation](https://docs.rs/time/)
- [Official Rust Book - Dates and Times](https://doc.rust-lang.org/book/ch16-03-cargo-and-crates-io.html)