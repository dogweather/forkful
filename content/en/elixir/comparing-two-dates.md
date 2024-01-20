---
title:                "Comparing two dates"
html_title:           "Arduino recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Comparison of Dates in Elixir

## What & Why?

Comparing two dates means determining if one date is earlier, later, or the same as the other. Programmers do this to manipulate and analyze time-sensitive data effectively. 

## How to:

In Elixir, we use the `Date.compare/2` function of the `Date` module to compare dates. The function returns `:lt` for a lesser date, `:gt` for a greater date, and `:eq` for equal dates.

```Elixir
date1 = ~D[2022-01-01]
date2 = ~D[2022-02-01]

IO.inspect Date.compare(date1, date2) # {:lt, {2022, 1, 1}, {2022, 2, 1}}
```

## Deep Dive

Elixir, being a functional language, doesn't have native date types until version 1.3. Before this, we had to use external libraries like `Timex`. 

Instead of `Date.compare/2`, you could use `Date.diff/2` to get the difference in days, negative if the first date is earlier:

```Elixir
date1 = ~D[2022-02-01]
date2 = ~D[2022-01-01]

IO.inspect Date.diff(date1, date2) # 31
```

`Date.compare/2` uses Erlang's Calendar.ISO, which handles corner cases of Gregorian Calendar, making it accurate and efficient.

## See Also

- [Official Elixir's Date Documentation](https://hexdocs.pm/elixir/Date.html)
- [Comparison Operators in Elixir](https://hexdocs.pm/elixir/operators.html)
- [Timex Library](https://hexdocs.pm/timex/readme.html)