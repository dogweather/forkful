---
title:                "Comparing two dates"
html_title:           "Gleam recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Let's Talk About Date Comparison in Gleam 

## What & Why?

Comparing two dates is figuring out which comes first, or if they're the same. Programmers do this to sort events, calculate durations, or check deadlines.

## How to:

In Gleam, dates are compared using the standard comparison operators.

```Gleam
import gleam/date.{from_timestamp, Date}
import gleam/order.{Order}

fun compare_dates() {
    let date1 = from_timestamp(1609459200) 
    let date2 = from_timestamp(1612137600) 

    assert Ok(date1) = from_timestamp(1609459200)
    assert Ok(date2) = from_timestamp(1612137600)

    case Order.compare(date1, date2) {
        Equal -> "Dates are equal"
        Lt -> "Date1 is earlier than Date2"
        Gt -> "Date1 is later than Date2"
    }
}
```

In this above example, we compare two sets of Unix timestamps.

## Deep Dive

Comparing dates has been a common practice since early programming. Historical context shows it's critical in time-based computing tasks. In Gleam, implementation uses the Erlang :calendar module for date operations under the hood.

Various alternatives exist for date comparison especially with convenience libraries or in languages with built-in date types. The Date module in Gleam handles the basic comparison and arithmetic tasks using the standard comparison operators.

## See Also:

For more about date comparison in Gleam and for further reading:

1. Gleam documentation - [Gleam API](https://hexdocs.pm/gleam_stdlib/gleam/date.html)
2. Erlang Module - [:calendar](http://erlang.org/doc/man/calendar.html) 
3. Examples and Tutorials - [Gleam School](https://gleam.run/tour/)