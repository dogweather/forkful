---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "Elixir: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Konwersja daty do ciągu znaków jest jedną z podstawowych operacji w programowaniu. Jest to proces przekształcania daty w formę czytelną dla komputera, czyli w ciąg znaków. Programiści często wykonują tę operację, aby wyświetlić datę w przyjaznym dla użytkownika formacie lub aby przetwarzać dane w aplikacji.

## Jak to zrobić:

Aby przekonwertować datę do ciągu znaków w Elixir, użyjemy funkcji `DateTime.to_string/1`. Przykładowy kod wygląda tak:

```elixir
date = DateTime.utc_now()
DateTime.to_string(date)
```

Wynik będzie wyglądał mniej więcej tak: `"2021-04-23 14:25:45.151176Z"`. Możemy także określić własny format daty, korzystając z opcjonalnego parametru funkcji `to_string/2`:

```elixir
date = DateTime.utc_now()
DateTime.to_string(date, "{YYYY}-{M}-{D}")
```

W tym przypadku wynik będzie prezentowany w formacie `"2021-4-23"`.

## Deep Dive:
W przeszłości istniało wiele różnych formatów zapisu daty, ale obecnie najczęściej stosowany jest standard ISO 8601, który ma formę `YYYY-MM-DDTHH:MM:SS.mmmmmmZ`. Dzięki temu standardowi, maszyny są w stanie bezbłędnie przetwarzać daty i nie dochodzi do nieporozumień, które mogłyby mieć miejsce przy użyciu innych formatów.

Alternatywne sposoby konwersji daty do ciągu znaków w Elixir to użycie funkcji `~D`, która konwertuje datę do formatu `"YYYY-MM-DD"` lub `~T`, która konwertuje czas do formatu `"HH:MM:SS.mmmmmm"`. Możemy też użyć biblioteki `Timex`, która oferuje bardziej zaawansowane możliwości formatowania daty.

## Zobacz również:
Dokumentacja Elixir na temat konwersji daty: 
https://hexdocs.pm/elixir/DateTime.html#to_string/1
Dokumentacja Elixir na temat standardów ISO 8601: 
https://en.wikipedia.org/wiki/ISO_8601
Biblioteka Timex do zarządzania datami i czasem w Elixir: 
https://github.com/bitwalker/timex