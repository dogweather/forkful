---
title:                "Calculating a date in the future or past"
date:                  2024-01-20T17:30:55.291296-07:00
model:                 gpt-4-1106-preview
simple_title:         "Calculating a date in the future or past"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calculating a future or past date means figuring out what the date will be after or before a certain amount of time. Programmers do this for things like scheduling events, subscription renewals, or setting expiry dates.

## How to:
Gleam doesn't come with a built-in date/time library, so we'll use the `chronotope` library to handle dates and times. First, add `chronotope` to your `gleam.toml`:

```toml
[dependencies]
chronotope = "~> 0.4"
```

Now, let's do some date calculations:

```gleam
import chronotope
import chronotope.duration
import chronotope.date

fn calculate_date() {
  let now = chronotope.now()
  let two_weeks = duration.of_weeks(2)
  let future_date = date.add(now, two_weeks)
  let past_date = date.subtract(now, two_weeks)
  future_date, past_date
}

fn main() {
  let (future, past) = calculate_date()
  io.println(future)
  io.println(past)
}
```

Run it:

```bash
$ gleam run
```

Sample output might be:

```
2023-04-28
2023-03-31
```

## Deep Dive
In computing, date manipulation is part of the broader field of temporal databases and time-based data. In the 1970s, with mainframe computers as the mainstay, precise date and time tracking was already essential for functions like job scheduling.

As for alternatives, while `chronotope` is a solid choice in Gleam, other languages might use standard libraries like Python's `datetime` or JavaScript's `Date` object. The implementation across languages differ, but mostly they calculate dates by manipulating milliseconds since a known epoch (usually January 1, 1970, UTC).

Under the hood, `chronotope` manages dates as structs and performs calculations by converting intervals to a compatible unit (say, seconds or days), doing math on it, and converting it back to a date or time struct. This process accounts for quirks in calendars and time zones, which aren't always linear or consistent due to leap years, daylight saving time, and other anomalies.

## See Also
- [Temporal databases on Wikipedia](https://en.wikipedia.org/wiki/Temporal_database)
- [History of Timekeeping Devices](https://en.wikipedia.org/wiki/History_of_timekeeping_devices)