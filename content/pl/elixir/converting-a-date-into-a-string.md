---
title:                "Elixir: Konwertowanie daty na ciąg znaków"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Dlaczego Konwertować Datę na String?

Konwertowanie daty na string jest częstym zadaniem w programowaniu, szczególnie w Elixirze. Jest to przydatne, gdy chcemy wyświetlić datę w formacie, czytelnym dla ludzi, lub gdy potrzebujemy przekazać ją do innej funkcji lub systemu. W tym artykule dowiesz się, jak wykonać tę konwersję w Elixirze.

## Jak to zrobić?

Możemy skorzystać z funkcji `to_string` wraz z biblioteką `Calendar` w celu konwersji daty na string. Oto przykład:

```Elixir
date = ~D[2021-10-15]
Calendar.to_string(date, {:month, :day, :year})
```

Output: `"October 15, 2021"`

Możemy także podać format daty jako drugi argument w funkcji `to_string`, na przykład:

```Elixir
date = ~D[2021-10-15]
Calendar.to_string(date, "YYYY-MM-DD")
```

Output: `"2021-10-15"`

## Deep Dive

Warto pamiętać, że w Elixirze daty są przechowywane jako tuplet danych `{:calendar, {year, month, day}}`. Dzięki temu możemy łatwo wydobyć poszczególne elementy daty i wykorzystać je w konwersji do stringa. Przykładowo:

```Elixir
date = {Calendar.ISO, {2021, 10, 15}}
"#{elem(date, 1)}-#{elem(date, 2)}-#{elem(date, 3)}"
```

Output: `"2021-10-15"`

Możemy także użyć funkcji `to_string` na poszczególnych elementach daty, na przykład `elem(date, 2) |> to_string` lub `elem(date, 3) |> to_string`.

# Zobacz także

- [Dokumentacja Elixir - Calendar](https://hexdocs.pm/elixir/Calendar.html)
- [Poradnik Elixir School - Dates and Times](https://elixirschool.com/en/lessons/advanced/dates/)
- [Konwersja daty na string w Elixirze - blog post (po angielsku)](https://blog.usejournal.com/how-to-convert-dates-to-strings-in-elixir-525f9e7c7a71)