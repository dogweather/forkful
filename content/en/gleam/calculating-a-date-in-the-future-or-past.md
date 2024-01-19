---
title:                "Calculating a date in the future or past"
html_title:           "Gleam recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

"Calculating a date in the future or past" refers to time manipulation in programming. Programmers do it to perform tasks like event scheduling, reminders, or data analysis.

## How to:

Gleam currently doesn't have a built-in date/time library, therefore, you would typically need to use the Erlang runtime to perform such operations. Let's see how we can add days to the current date.

```erlang
import erlang
import erlang/time.{Now}

pub fn add_days_to_current_date(days: Int) {
  current_date = Now.milliseconds() 
  future_date = current_date + (days * 24 * 60 * 60)
  future_timestamp = erlang:system_time_to_universal_time(future_date)
  future_timestamp
}
```

In the above code, `Now.milliseconds()` gets the time in milliseconds since Unix Epoch. We're adding to it the number of milliseconds equivalent to the number of days passed. `erlang:system_time_to_universal_time/1` function, converts it back to a date-time tuple we can interpret more easily.

## Deep Dive

Manipulation of dates dates back to the inception of structured programming, with increasing sophistication as more complex needs become evident in technology evolution. Alternatives within the BEAM languages, which Gleam is a part of, include the Erlang date/time library and Elixir's Timex. In Gleam, as it does not yet have a built-in functionality, we utilise the Erlang libraries as shown above under the interoperability premise.

## See Also

* Erlang's date/time library: http://erlang.org/doc/man/erlang.html#type-system_time
* Elixir's Timex library: https://hexdocs.pm/timex/readme.html
* More on Gleam's Interoperability with Erlang: https://gleam.run/book/tour/interop-with-erlang.html