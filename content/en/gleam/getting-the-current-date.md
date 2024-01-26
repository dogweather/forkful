---
title:                "Getting the current date"
date:                  2024-01-20T15:14:29.001247-07:00
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

In programming, snagging the current date means to fetch the real-world, right-now date. We do this for logging events, timestamps for transactions, or just to schedule stuff.

## How to:

```Gleam
import gleam/calendar.{Date, now}
import gleam/io

pub fn main() {
  let today: Date = now
  io.println(today)
}
```

Sample Output:

```
Date(year: 2023, month: 4, day: 14)
```

## Deep Dive

The concept of retrieving the current date is as old as computing itself. It's rooted in the need to relate computing tasks to real-time events. In Gleam, the `calendar` module makes working with dates a breeze, offering types and functions such as `Date` and `now`.

Before modules like this existed, developers often interfaced directly with the operating system to fetch dates. This could be fiddly and error-prone.

The `Date` type in Gleam is a straightforward tuple struct, giving you the year, month, and day components, respectively. Behind the scenes, `now` will generally call the appropriate system-level APIs to get the date for you, abstracting away the platform-specific differences.

Alternatives for more complex date and time handling might involve using external packages, as Gleam's standard library is intentionally kept minimal. These can provide additional functionality like time zones, formatting, and parsing.

## See Also

- Gleam `calendar` module documentation: https://hexdocs.pm/gleam_stdlib/gleam/calendar/
- Documentation on Erlang's time functions, which Gleam may depend on: https://erlang.org/doc/man/erlang.html#date-0
- For more advanced date-time libraries, the Elixir community's offerings, such as 'Timex', could be reviewed for potential interoperability: https://hex.pm/packages/timex
