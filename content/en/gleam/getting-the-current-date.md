---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date refers to a program retrieving the system's present date. Programmers often do this to track real-time events, deadlines, and as tags for logging errors or activities. 

## How To:

As of the time of writing, the Gleam language is on version 0.14 which doesn't have native support for date and time. A workaround is interfacing with Erlang's date library. Here's how:

```Gleam
import erlang/time.{Now as now}

fn get_current_date() {
  let {y, m, d} = now()
  y + "-" + m + "-" + d
}

// Prints: "2022-03-14"
io.println(get_current_date())
```
Notice how `now()` gives us the year, month, and day. We concatenate them using "+".

## Deep Dive 

Historically, virtually every programming language has incorporated the ability to fetch the current date because it's a widespread need. Erlang is no different: the 'time' library is the go-to resource in the Erlang world, which Gleam uses heavily in its operation.

Alternative methods are often library-dependent. So, you could say that the Erlang time library is Gleam's "alternative." Plus, various external datetime libraries can work across different languages that support interaction with Gleam.

Last but not least, implementation relies on fetching the system's present datetime, then processing that data into a more palatable date format such as year-month-day.

## See Also

For more details on date manipulation in Gleam and interacting Gleam with Erlang, check out these valuable resources:

- Gleam's official GitHub repo: https://github.com/gleam-lang/gleam
- Gleam's guide to calling Erlang: https://gleam.run/book/tour/erlang.html
- Erlang’s time module Doc: http://erlang.org/doc/man/erlang.html#type-time