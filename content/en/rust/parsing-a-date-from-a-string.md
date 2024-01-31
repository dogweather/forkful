---
title:                "Parsing a date from a string"
date:                  2024-01-20T15:38:08.957602-07:00
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing a date from a string means converting text into a date format your code can understand. We do this because dates often come as strings from user input or external data sources and we need them in a structured form for computation and storage.

## How to:
To parse dates in Rust, we use the `chrono` crate, a go-to library for date and time.

First, add `chrono` to your `Cargo.toml`:

```toml
[dependencies]
chrono = "0.4"
```

Then, here's a simple example of parsing an ISO 8601 date:

```rust
extern crate chrono;
use chrono::prelude::*;

fn main() {
    let date_str = "2023-04-05";
    let parsed_date = date_str.parse::<NaiveDate>().unwrap();

    println!("Parsed date is: {}", parsed_date);
}

```
Output:
```
Parsed date is: 2023-04-05
```

## Deep Dive
`chrono` is Rust's choice for date and time parsing, pretty much since Rust's inception. Before `chrono`, Rust had a basic time library, but it lacked features. `chrono` filled that gap.

For alternatives, you've got `time` crate, but `chrono` wins in popularity and feature set. Implementation-wise, parsing a date string involves specifying the format and handling the possibility of failure—that's why we used `unwrap()`, which is fine in examples but use `match` or `unwrap_or_else` in real code to handle errors gracefully.

Historically, programming languages have struggled with date and time. It's complex due to leap years, time zones, and daylight saving changes. That's why crates like `chrono` are valuable—they handle the oddities for us.

## See Also
- Official `chrono` crate documentation: https://docs.rs/chrono/
- Rust API guidelines about error handling: https://rust-lang.github.io/api-guidelines/error.html
- An in-depth look at Rust's time library history: https://www.reddit.com/r/rust/comments/2z54zb/history_of_rusts_time_library/
