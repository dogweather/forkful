---
title:                "Converting a date into a string"
html_title:           "Gleam recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string transforms a date object, readable by a computer's system, into a text format understandable by humans. This is often done by programmers to display date information in a user interface or when saving it in a format that doesn't support date objects.

## How to:

In Gleam, you'd utilize the built-in Date and Time modules to convert dates into strings. Here is a basic example:

```Gleam
import gleam/date.{Date}
import gleam/string.{from_date}

fn main() {
  let d = date.new(2022, 8, 12)
  let date_string = string.from_date(d)

  assert date_string == "2022-08-12"
}
```
In this code, a Date object `d` is created for August 12, 2022. This date is then converted into string using `from_date` function, resulting in "2022-08-12".

## Deep Dive

Converting dates into strings is an old but vital problem in computing. In the past, each language or system had its own way of representing dates and times. This had led to issues, notably the Y2K bug. Thankfully, ISO 8601 (International Standards Organization) introduced a standardized method for date and time representation.

In a language like Gleam, if you desired a different formatting, you'd have to write your own conversion function. For example, representing the format as "Month DD, YYYY."

The conversion to a string is efficient within Gleam due to its statically-typed nature and the Erlang VM upon which it runs.

## See Also

Get more info about Gleam's in-built string and date functions at: 
- `gleam/string`: (https://hexdocs.pm/gleam_stdlib/gleam/string/)
- `gleam/date`: (https://hexdocs.pm/gleam_stdlib/gleam/date/)
For info on ISO 8601: (https://www.iso.org/iso-8601-date-and-time-format.html)