---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:13:54.167908-07:00
simple_title:         "現在の日付を取得する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
Current date retrieval means fetching the present day's date. We do this to timestamp events, handle schedules, or for logging purposes in Elixir programs.

## How to: (どうやって：)
Elixir's `Date` module is straightforward. Here's how you get the current date:

```elixir
today = Date.utc_today()
IO.inspect(today)
```

Output:

```
~D[2023-04-05]
```

## Deep Dive (詳細な分析)
The `Date` module in Elixir has been a core part since its early days and aligns with its immutable data structure philosophy. This module handles dates without times. In contrast, the `DateTime` module manages dates with times.

For just the date, `Date.utc_today/0` is perfect. It returns the current date in the UTC timezone. If you need time, or other time zones, `DateTime` and `Time` modules are where you’d look next.

Before Elixir 1.3, we relied on external libraries like Timex for date-time operations. Now, it's baked into the language.

Elixir treats date-time data as a first-class citizen. It's leveraging Erlang's powerful BEAM VM, ensuring robustness and concurrency-friendly operations.

## See Also (関連情報)
1. Elixir's `Date` module documentation: [hexdocs.pm/elixir/Date.html](https://hexdocs.pm/elixir/Date.html)
2. Erlang's calendar module for understanding underlying implementation: [erlang.org/doc/man/calendar.html](https://www.erlang.org/doc/man/calendar.html)
3. 'Timex', a rich third-party library for Elixir: [hex.pm/packages/timex](https://hex.pm/packages/timex)
