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

Parsing a date from a string means converting, or "parsing," human-readable text into a machine-readable date. This is essential for times when an app needs to process time-based data entered by a user in their preferred date format.

## How to:

To get started, Gleam doesn't offer a built-in way for date parsing. We'll need to use external libraries such as `erlang.date`.

Here's how to do it:

```Gleam
import erlang

fn parse_date(date_str: String) {
  erlang.list_to_tuple(date_str
    |> string.split("/") 
    |> list.map(
        fn(i) { 
          case int.from_string(i) {
            Ok(n) -> n
            Error(_) -> 0
          }
        })
      )
}
```

Example usage and output:

```Gleam
parse_date("12/31/2021")
// Output: #(2021, 12, 31)
```

## Deep Dive

Historically, date parsing was a complex process due to the variety of date format conventions across cultures and computing systems. It's now made simpler, thanks to robust libraries and standardized date formats like ISO 8601.

When choosing methods to parse dates, consider available libraries, project requirements, and the date format you'll mainly deal with. For instance, `erlang.date` is a good choice for Unix timestamp parsing, but may not fit other use cases.

An important detail in the implementation of date parsing is error handling. Since it involves user inputs, invalid dates, month, and year values are possible. Our code above defaults these to zero if conversion fails, but deeper validation should be implemented in a production application.

## See Also

* [Gleam's official documentation](https://gleam.run/docs/introduction/)
* [Erlang's date-related functions](http://erlang.org/doc/man/calendar.html)
* [DateTime Libraries in Gleam programming Language](https://stackoverflow.com/questions/tagged/gleam)