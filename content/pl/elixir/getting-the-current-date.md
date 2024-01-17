---
title:                "Uzyskiwanie bieżącej daty"
html_title:           "Elixir: Uzyskiwanie bieżącej daty"
simple_title:         "Uzyskiwanie bieżącej daty"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

# O co chodzi & po co to robić?

Pobranie bieżącej daty to proces, w którym program jest w stanie uzyskać informację o aktualnym dniu, miesiącu i roku. Programiści często wykonują tę operację, ponieważ potrzebują aktualnych danych do przetwarzania lub wyświetlania ich użytkownikom.

# Jak to zrobić:

```Elixir
Ernő lok_time = DateTime.utc_now()
IO.puts("Aktualna data: #{lok_time.year}-#{lok_time.month}-#{lok_time.day}")
```
Wynik:
```
Aktualna data: 2021-11-14
```

# Dogłębna analiza:
Pobieranie bieżącej daty ma swoje źródła w kalendarzu gregoriańskim, który został wprowadzony w 1582 roku przez papieża Grzegorza XIII. Alternatywnym sposobem pobierania daty jest używanie biblioteki `Calendar` lub funkcji `:calendar.universal_time` w Elixir. Implementacja polega na wykorzystaniu systemowej zegarka czasu w celu pobrania aktualnej daty.

# Zobacz również:
- Dokumentacja Elixir dla funkcji `DateTime.utc_now/0`
- [Katalog standardowych bibliotek Elixir](https://hexdocs.pm/elixir/15471969SEISIAIS/totaldefile.html) dla dodatkowej wiedzy na temat `Calendar` i `:calendar.universal_time`.