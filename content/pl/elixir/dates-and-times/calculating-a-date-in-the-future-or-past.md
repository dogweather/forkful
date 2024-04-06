---
date: 2024-01-20 17:30:55.747152-07:00
description: "How to: G\u0142\u0119bsze zanurzenie: W Elixirze, modu\u0142 `Date`\
  \ jest kluczowy do pracy z datami. Historia koncepcji operacji na datach si\u0119\
  ga kalendarzy i potrzeby\u2026"
lastmod: '2024-04-05T22:50:49.363639-06:00'
model: gpt-4-1106-preview
summary: "G\u0142\u0119bsze zanurzenie: W Elixirze, modu\u0142 `Date` jest kluczowy\
  \ do pracy z datami. Historia koncepcji operacji na datach si\u0119ga kalendarzy\
  \ i potrzeby \u015Bledzenia czasu. Alternatywy do `Date.add` to u\u017Cycie paczek\
  \ zewn\u0119trznych jak `Timex`, kt\xF3re dodaj\u0105 dodatkow\u0105 funkcjonalno\u015B\
  \u0107. Elixir u\u017Cywa kalendari\xF3w, by obs\u0142u\u017Cy\u0107 r\xF3\u017C\
  ne systemy datowania, a `Date.add` operuje w kontek\u015Bcie kalendarza Gregoria\u0144\
  skiego."
title: "Obliczanie daty w przysz\u0142o\u015Bci lub przesz\u0142o\u015Bci"
weight: 26
---

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
