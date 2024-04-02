---
date: 2024-01-20 17:37:18.587561-07:00
description: "Converting a date to a string in Rust lets us display dates in a human-readable\
  \ format. We do this for UIs, logs, or any place where people need to make\u2026"
lastmod: '2024-03-13T22:44:59.906483-06:00'
model: gpt-4-1106-preview
summary: "Converting a date to a string in Rust lets us display dates in a human-readable\
  \ format. We do this for UIs, logs, or any place where people need to make\u2026"
title: Converting a date into a string
weight: 28
---

## What & Why?

Converting a date to a string in Rust lets us display dates in a human-readable format. We do this for UIs, logs, or any place where people need to make sense of dates.

## How to:

Rust's `chrono` crate is the go-to for date and time handling. Make sure it's in your `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Now, let's format a date as a string.

```rust
extern crate chrono;
use chrono::{DateTime, Utc, NaiveDateTime};

fn main() {
    let date: DateTime<Utc> = Utc::now(); // Get current UTC date and time.
    let formatted_date = date.format("%Y-%m-%d %H:%M:%S").to_string();
    println!("{}", formatted_date); // Prints: 2023-03-15 14:30:45
}
```

## Deep Dive

Before `chrono`, Rust's standard library had a few date and time functions, but they were basic. `chrono` built on that bedrock to offer comprehensive functionality. An alternative might be Rust's new `time` crate, aiming for a safer and more ergonomic API.

When you convert a date to a string, you're serializing – turning data into a format that can be shared or stored. The format you choose (`%Y-%m-%d %H:%M:%S` in our case) is up to you, and `chrono` supports many such patterns.

Internally, dates are often stored as timestamps – seconds from a starting point, like Unix epoch (January 1, 1970). When you format a date, you compute the human-readable form from this count, considering time zones and leap seconds.

## See Also

- `chrono` crate documentation: https://docs.rs/chrono/
- Rust's `time` crate documentation: https://docs.rs/time/
- Date formatting syntax: http://www.unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table
