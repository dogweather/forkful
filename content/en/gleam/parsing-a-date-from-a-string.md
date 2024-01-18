---
title:                "Parsing a date from a string"
html_title:           "Gleam recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the process of extracting the date information (e.g. day, month, year) from a string of characters. This is useful for programmers as it allows them to convert date inputs in various formats to standardized values that can be easily manipulated and compared in their code.

## How to:

To parse a date from a string in Gleam, we can use the `Date.from_text` function. This function takes in a string and returns a `Date` record with the corresponding date information.

```
Gleam ... 
let date = Date.from_text("10/12/2021")
```
The `date` variable will now hold a `Date` record with the day as `10`, month as `12`, and year as `2021`.

We can also use the `format` parameter to specify the format of the input string. For example:
```
let date = Date.from_text("Dec 03, 2021", "%b %d, %Y")
```
In this case, the `format` parameter follows the same rules as `strftime` in the C programming language.

## Deep Dive:

Parsing a date from a string has been a common task for many programming languages, and it is no different in Gleam. In fact, many other programming languages have similar functionalities for parsing dates, such as `dateutil` in Python and `DateTime::parse` in Ruby.

Besides the `format` parameter, the `Date.from_text` function in Gleam also has an optional `locale` parameter for dealing with different date formats in different languages.

Internally, `Date.from_text` uses the `DateTime.Parse` library, which leverages the power of the `DateTime::parse` function in Ruby and Rust's `chrono` library.

## See Also:

- [Gleam's Date module documentation](https://gleam.run/stdlib/date.html)
- [Python's dateutil documentation](https://dateutil.readthedocs.io/en/stable/)
- [Ruby's DateTime::parse documentation](https://ruby-doc.org/stdlib/libdoc/date/rdoc/DateTime.html#method-c-parse)
- [Rust's chrono library documentation](https://docs.rs/chrono/latest/chrono/)