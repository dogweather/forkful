---
title:                "Calculating a date in the future or past"
html_title:           "Elixir recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?

Calculating a date in the future or past allows prediction or examination related to specific occurrences. Programmers use it to handle business requirements like calculating invoice due dates, subscriptions, system logs, etc.

## How to:

In Elixir, you accomplish this using `Date.add/2` & `Date.diff/2` from its built-in `Date` module.

```Elixir
# Calculate a date 30 days in the future
future_date = Date.add(Date.utc_today, 30)
IO.inspect(future_date)

# Calculate days past since a particular date
past_days = Date.diff(Date.utc_today, ~D[2022-04-01])
IO.puts(past_days)
```

Which would output:

```Elixir
~D[2022-05-02]
12
```

## Deep Dive:

Historically, calculating dates in Elixir heavily relied on Erlang's library or external libraries like Timex, but thanks to advances in Elixir's standard library, this task got easier.

If `Date.add/2` or `Date.diff/2` don't cater to your needs, Elixir allows for more complex manipulations. For example, you can manipulate desired parts of a date separately using `Date.replace/2`. 

Implementation of date calculation in Elixir is precise, following ISO 8601 standard and handling leap years, GMT offsets, etc., by default. However, be aware of time zone issues - Elixir's Date functions use UTC timezone as default, so consider current timezone if needed.

## See Also:

- Elixir’s official documentation: [Date](https://hexdocs.pm/elixir/Date.html)
- Practical Elixir use-cases: [Dates and times in Elixir](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)