---
title:                "Obliczanie daty w przyszłości lub przeszłości"
date:                  2024-01-20T17:30:55.747152-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Co to jest i dlaczego?

Obliczanie daty w przyszłości albo przeszłości to ustalanie nowych dat na podstawie zadanej. Programiści robią to dla organizacji wydarzeń, planowania zadań, przypomnień czy też wygaśnięcia sesji.

## How to:
Jak to zrobić:

```
# Dodawanie dni
date = ~D[2023-04-01]
future_date = Date.add(date, 10)
IO.puts(Date.to_string(future_date)) # 2023-04-11

# Odejmowanie dni
past_date = Date.add(date, -20)
IO.puts(Date.to_string(past_date)) # 2023-03-12
```

## Deep Dive:
Głębsze zanurzenie:

W Elixirze, moduł `Date` jest kluczowy do pracy z datami. Historia koncepcji operacji na datach sięga kalendarzy i potrzeby śledzenia czasu. Alternatywy do `Date.add` to użycie paczek zewnętrznych jak `Timex`, które dodają dodatkową funkcjonalność. Elixir używa kalendariów, by obsłużyć różne systemy datowania, a `Date.add` operuje w kontekście kalendarza Gregoriańskiego.

## See Also:
Zobacz także:

- [Elixir Documentation for Date](https://hexdocs.pm/elixir/Date.html)
- [Hex.pm package for Timex](https://hex.pm/packages/timex)
- [Elixir School: Dates and Times](https://elixirschool.com/en/lessons/basics/date-time/)