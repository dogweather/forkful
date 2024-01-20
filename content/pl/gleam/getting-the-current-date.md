---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:14:36.274154-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Getting the current date means fetching the present day, month, and year. Programmers need this info for timestamps, logs, or any feature that records when an event occurred.

## How to: (Jak to zrobić:)
In Gleam, we rely on Erlang functions because Gleam runs on the BEAM VM like Erlang does. Here's a simple example:

```Gleam
import gleam/erlang
import gleam/calendar.{Date}

fn main() {
  let today: Result(Date, _) = erlang.date()
  case today {
    Ok(date) -> date
    Error(_) -> "Couldn't fetch the date."
  }
}
```

Output might look like:
```
#(2023, 3, 14)
```

## Deep Dive (Dogłębna Analiza)
Historically, Gleam builds on the shoulders of Erlang, which started in the '80s for telecoms. Unlike some languages with built-in date functions, Gleam imports Erlang's `:date` for the job.

Alternatives? You could talk to an external API or use another library that gives you more features.

As for implementation, remember that `erlang.date()` could return an error if it fails for some reason. That's why we match on `Result` and handle the potential error.

## See Also (Zobacz Także)
- Gleam's official docs on Date and Time: https://hexdocs.pm/gleam_stdlib/gleam/calendar/
- Erlang's `:date` function details: http://erlang.org/doc/man/erlang.html#date-0
- Learn more about Erlang's history: https://www.erlang.org/course/history