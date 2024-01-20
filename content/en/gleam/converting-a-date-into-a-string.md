---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a date into a string in programming allows for easy manipulation and display of date information. Programmers do it to improve user interaction and to facilitate date comparisons in code logic.

## How to:
Alright, let's dive into some code. Here’s an example of how to convert a date into a string using Gleam.

```Gleam
import gleam/date
import gleam/date.{Date}
import gleam/string.{from_utf8}

fn date_to_string(d: Date) -> String {
  let Date(year, month, day) = d
  from_utf8(<<year, "-", month, "-", day>>)
}

fn main(_) {
  let d = date.new(2022, 11, 23)
  let date_string = date_to_string(d)
  io.println(date_string)
}
```
When you run this program, it should print out "2022-11-23" to the console.

## Deep Dive

Historically, all computer systems didn't share the same methods for representing dates and managing time, making conversion into a standardized format like a string crucial for interoperability.

As for alternatives, there's a multitude of ways you can format a date string depending on the exact requirements of your program or the locale of your users. Some other popular formats include "23-Nov-2022" or "23/11/2022".

The implementation details of date conversion to a string are depending on the language and libraries you're using. Gleam’s implementation uses the underlying Erlang and uses the from_utf8 method to convert a binary to a string, which is constructed from parts of a date value.

## See Also

For a more in-depth look into dates and times in Gleam, check out [Gleam's date documentation](https://hexdocs.pm/gleam_stdlib/gleam/date.html).

Not satisfied with the date format? Check out how to implement your own date-to-string conversion function in this [Gleam format conversion guide](https://gleam.run/tour/basic-types/).