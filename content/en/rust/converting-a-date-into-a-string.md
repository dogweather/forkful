---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string in programming, essentially, changes the date datatype into a string datatype. The need for this lies in displaying the date in a more human-readable format or transferring data across different systems.

## How To:

In Rust, the `chrono` crate provides functionalities to convert dates to string and vice versa. Here's an example:

```Rust
use chrono::{DateTime, Utc};

fn main() {
    let date: DateTime<Utc> = Utc::now();
    println!("{}", date.to_rfc3339());
}
```
When you run this code, the output will look something like "2022-01-01T12:34:56.789Z".

## Deep Dive

In the past, Rust didn't have built-in support for manipulating dates and times, that is why the community developed the `chrono` crate to meet this need. 

Alternatives to `chrono` include the `time` crate for time manipulation and parsing, but is less user-friendly.

The `chrono` crate keeps date and time data in a struct, and the `to_rfc3339()` method formats a string according to the Internet standard RFC 3339. It ensures consistent date-time representation.

## See Also

- Chrono Documentation: [https://docs.rs/chrono/0.4.19/chrono/](https://docs.rs/chrono/0.4.19/chrono/)
- Time Crate Documentation: [https://docs.rs/time/0.3.5/time/](https://docs.rs/time/0.3.5/time/)
- RFC 3339 Standard: [https://tools.ietf.org/html/rfc3339](https://tools.ietf.org/html/rfc3339)