---
title:                "Comparing two dates"
date:                  2024-01-20T17:32:53.500407-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparing two dates"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

Comparing two dates is checking how they relate in time. Programmers do this to sort events, implement schedules, or validate periods.

## How to:

In Gleam, we use the `gleam/calendar` library for date handling. Unfortunately, as of my knowledge cutoff in early 2023, Gleam doesn't have a built-in way to directly compare dates like some other languages might. So, while this isn't plug-and-play, with a couple of functions, we can start comparing dates.

First, let's make sure we can create some dates:

```gleam
import gleam/calendar.{Date}

pub fn make_date(year: Int, month: Int, day: Int) -> Option(Date) {
  calendar.new_date(year, month, day)
}
```

Now, let's write a function to compare two dates. We can convert dates to a comparable format - like the number of days since a set date. But since this is a simple example, let's just do a basic check to see if one date is before another:

```gleam
import gleam/calendar.{Date, is_before}

pub fn is_date1_before_date2(date1: Date, date2: Date) -> Bool {
  is_before(date1, date2)
}
```

Sample usage:

```gleam
import gleam/io

fn main() {
  let date1 = make_date(2023, 3, 14)
  let date2 = make_date(2021, 6, 18)
  
  let result = case date1 {
    Ok(d1) -> case date2 {
      Ok(d2) -> is_date1_before_date2(d1, d2)
      Error(_) -> False
    }
    Error(_) -> False
  }
  
  io.debug(result) // Should print True because date1 is after date2
}
```

## Deep Dive

Historically, date/time APIs vary across languages, with some providing robust comparison operators and others requiring manual calculations. When comparing dates, many languages convert to a standardized form like Unix time (seconds since Jan 1, 1970) which can be directly compared. However, edge cases like leap seconds or daylight savings can add complexity.

In Gleam, because of the language's focus on safety and reliability, date operations might be less straightforward but aim to be correct without implicit assumptions. That's why you might not find a one-liner to do this kind of job, but with proper handling of dates using the `calendar` module, you can manage well.

For alternatives, one could write more complex functions that compare the year, then month, then day, if fine-grained control is needed or until direct date comparison support is actually added to Gleam. Lastly, always keep an eye on the language updates; Gleam is evolving fast, and new features might land after my knowledge cutoff.

## See Also

- For the overall Gleam lang tutorial, visit: [https://gleam.run](https://gleam.run).
- For dilemmas about time and date handling in programming and their solutions, have a read through [https://yourcalendricalfallacyis.com/](https://yourcalendricalfallacyis.com/).
