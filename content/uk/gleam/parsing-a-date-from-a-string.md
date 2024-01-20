---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:36:37.546021-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що & Чому?)
Parsing a date from a string is about converting text into a date format that a program can understand and work with. Programmers parse dates to handle and manipulate timelines, schedule events, and record time-stamped data.

## How to: (Як це зробити:)
Gleam doesn't have a built-in date parsing function, so let's use `chrono` library which provides date and time functionality. First, add it to your `gleam.toml` dependencies:

```toml
[dependencies]
chrono = "~> 0.1"
```
Here's a Gleam function to parse a date string:

```rust
import chrono

pub fn parse_date_string(date_string: String) -> Result(chrono.DateTime, String) {
  chrono.string_to_datetime(date_string)
}
```
And using the function:

```rust
pub fn main() {
  let date_result = parse_date_string("2023-04-01T12:30:00Z")
  case date_result {
    Ok(date) -> date
    Error(e) -> e
  }
}
```

Sample output might be a `DateTime` object or an error message.

## Deep Dive (Поглиблений аналіз):
Historically, date parsing was a mess with numerous formats and the Y2K bug. Now, standards like ISO 8601 help. Alternatives to `chrono` include writing custom parsers or using other libraries, but `chrono` is handy for its simplicity and coverage of common use cases. Parsing involves error-handling, as input might not match the expected format.

## See Also (Дивіться також):
- [ISO 8601 Date format guide](https://en.wikipedia.org/wiki/ISO_8601)