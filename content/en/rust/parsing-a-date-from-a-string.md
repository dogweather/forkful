---
title:                "Parsing a date from a string"
html_title:           "Rust recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the process of converting a date represented as a string into a date object that can be used in a program. This is commonly done in order to manipulate or compare dates in a more efficient manner. Programmers may need to parse dates in order to process user input, handle data from a database, or generate reports.

## How to:

Parsing dates in Rust is made simple with the help of the `chrono` crate. First, we need to add the following line to our `Cargo.toml` file: 

```Rust
[dependencies]
chrono = "0.4"
```

Then, in our `main.rs` file, we can use the crate to parse a date from a string. Here's an example:

```Rust
use chrono::prelude::*;

fn main() {
    // Parse a date in ISO 8601 format
    let date_str = "2020-06-15";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d").unwrap();

    println!("Parsed date: {}", date);
}
```

Running this code will output: `Parsed date: 2020-06-15`.

We can also specify a specific date and time format to parse from, like so:

```Rust
use chrono::prelude::*;

fn main() {
    // Parse a date and time in a custom format
    let datetime_str = "December 25, 2020 12:00 PM";
    let dt = NaiveDateTime::parse_from_str(datetime_str, "%B %d, %Y %I:%M %p").unwrap();

    println!("Parsed datetime: {}", dt);
}
```

The output will be: `Parsed datetime: 2020-12-25T12:00:00`.

## Deep Dive

Parsing dates has been a crucial aspect of programming since the dawn of computer systems. In the early days, dates were often represented in different formats which made it difficult to compare and manipulate them. The ISO 8601 standard was established in order to provide a unified way to represent dates, and it is still widely used today.

Aside from using a crate like `chrono`, programmers can also opt to parse dates using regular expressions. However, this method may be less efficient and can require more code to handle different date formats.

As for parsing implementation, `chrono` uses a combination of parsing and formatting codes to convert a string into a date object. These codes specify the expected format of the date string and allow for more flexibility in handling various date formats.

## See Also

- `chrono` crate [documentation](https://docs.rs/chrono/)
- Rust [dates and times](https://doc.rust-lang.org/std/time/index.html) guide