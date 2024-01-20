---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string transforms a readable date format into a machine-understood date object. Programmers use this to convert and manipulate data when dates are initially in string format.

## How to:

Here's a handy walk-through on how you can parse a date string in Rust:

```Rust
extern crate chrono;
use chrono::*;

let input = "2021-08-31 12:00:09";
let parsed_date = NaiveDateTime::parse_from_str(input, "%Y-%m-%d %H:%M:%S").unwrap();

println!("{:?}", parsed_date);
```

With the output as follows:

```Rust
NaiveDateTime(2021-08-31T12:00:09)
```

## Deep Dive

Historically, hand-coding a parsing mechanism to handle different date formats was a common thing. But bugs? Don't get me started. It was inefficient and error-prone which led to birth of libraries like `chrono`.

Now, you'd think, "Why not just use standard functions available in the language?" Well, Rust's standard library does not yet provide comprehensive date parsing and formatting capabilities. That's why `chrono` steps in. It's the de facto crate to manage dates and times in Rust. 

You could also go off the beaten path and use other crates like `time` or even `dateparser`, but `chrono` remains a strong favorite in the Rustacean's toolkit for its robustness and ease of use. 

## See Also:

For a deep-dive into the chrono feature-set, check out: [Chrono on crates.io](https://crates.io/crates/chrono)  
For more info about Rust's date and time handling, check out: [Rust Lang's documentation](https://doc.rust-lang.org/std/time/index.html)
And for alternatives, you might find: [Dateparser crate](https://github.com/stevedonovan/dateparser) and [Time crate](https://github.com/time-rs/time) useful.