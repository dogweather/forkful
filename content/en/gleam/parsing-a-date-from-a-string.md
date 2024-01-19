---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the process of converting textual data into a date object that a computer can understand and manipulate. This conversion is crucial for programmers dealing with user input or data from APIs where dates usually come in a string format.

## How to:

Working with date strings in Gleam requires the `gleam/calendar` library. Here's how you parse a date:

```Gleam
import gleam/calendar.{parse_date}

fn main() {
  let date_string = "2011-12-03"
  let parsed_date = parse_date(date_string)
  case parsed_date {
    Ok(date) -> 
      // use the date
      date.month
    Error(_) -> 
      // handle parsing error
      "Invalid date"
  }
}
```
After parsing, you can interact with the date using the returned date object. If the string is not a valid date, an error is returned.

## Deep Dive

The concept of parsing dates from strings originates from the necessity to work with human-readable dates in software development. Originally, dates were represented as strings as that was the format most familiar and readable to humans. With the rise of high-level programming languages, concrete date data types became prevalent and the need to convert between string and date representations became a common task. 

In Gleam, parsing a string date is performed using the `parse_date` function within the `gleam/calendar` library. It supports the ISO 8601 date format (YYYY-MM-DD). If needed, you could write your parsers using lower-level string processing functions or use external libraries for more complex formats.

## See Also

To delve more into Gleam's date parsing capabilities, check the official documentation at https://gleam.run/stdlib/calendar/

Additionally, for broader insights into Gleam's overall characteristics, visit Gleam's GitHub page at: https://github.com/gleam-lang/gleam.