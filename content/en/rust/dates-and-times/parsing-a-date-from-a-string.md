---
date: 2024-02-03 19:03:00.528809-07:00
description: 'How to: #.'
lastmod: '2024-03-13T22:44:59.904457-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Parsing a date from a string
weight: 30
---

## How to:


### Using Rust's Standard Library (`chrono` Crate)
The Rust standard library does not include date parsing directly, but the widely-used `chrono` crate is a robust solution for date and time manipulation. First, add `chrono` to your `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Then, use `chrono` to parse a date string into a `NaiveDate` object:

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("Failed to parse date");

    println!("Parsed date: {}", date);
}

// Sample Output:
// Parsed date: 2023-04-01
```

### Using Rust's Advanced Date-Time Handling (`time` Crate)
For more advanced date-time handling, including more ergonomic parsing, consider the `time` crate. First, include it in your `Cargo.toml`:

```toml
[dependencies]
time = "0.3"
```

Then, parse a date string using the `Date` type and `PrimitiveDateTime`:

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("Failed to parse date and time");

    println!("Parsed datetime: {}", parsed_date);
}

// Sample Output:
// Parsed datetime: 2023-04-01 12:34:56
```

Both examples showcase how Rust, with the aid of third-party crates, facilitates the parsing of date strings into manipulable date objects, making it a powerful tool for software development involving temporal data.
