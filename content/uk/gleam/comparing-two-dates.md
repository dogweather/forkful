---
title:                "Порівняння двох дат"
date:                  2024-01-20T17:33:16.556096-07:00
model:                 gpt-4-1106-preview
simple_title:         "Порівняння двох дат"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? | Що та Чому?

Comparing two dates lets us figure out their order and time difference. We do it to track time-based events, schedule tasks, or simply to log when something happens.

## How to: | Як це зробити:

Gleam's stdlib doesn't come with date types or functions out of the box, so we rely on external libraries. Here's a basic example using the `chronotope` crate, which is based on the Rust `chrono` crate.

```gleam
import chronotope
import chronotope.{datetime}

pub fn demo() {
  let date1 = datetime.from_iso_string("2023-03-01T00:00:00Z").unwrap()
  let date2 = datetime.from_iso_string("2023-03-10T00:00:00Z").unwrap()
  
  case datetime.compare(date1, date2) {
    Lt -> "date1 is earlier"
    Eq -> "dates are the same"
    Gt -> "date1 is later"
  }
}
```

Sample output for the `demo` would be `"date1 is earlier"` since March 1st is before March 10th.

## Deep Dive | Поглиблений Розгляд

Historically, date and time handling in programming can be messy due to time zones and leap seconds. Gleam, a statically-typed language that compiles to Erlang, doesn't try to reinvent the wheel. Instead, it provides the ability to tap into existing solutions from the Erlang ecosystem or use Rust libraries through Gleam's foreign function interface.

Alternatives for date comparison can involve using the Erlang `:calendar` module or other external libraries, each with their trade-offs in terms of functionality and ease of use.

When you compare two dates, what typically happens under the hood is a conversion to a consistent format, often Coordinated Universal Time (UTC), followed by a simple subtraction to find the difference.

## See Also | Дивіться Також

- Erlang's [:calendar module documentation](http://erlang.org/doc/man/calendar.html) for an alternative provided by the Erlang standard library.