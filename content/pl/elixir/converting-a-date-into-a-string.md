---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Konwersja daty na łańcuch znakowy to proces zamiany obiektu daty na czytelny dla człowieka format tekstowy. Programiści wykonują tę operację, aby łatwiej wyświetlać daty użytkownikom lub zapisywać je w bazach danych.

## Jak to zrobić:

W Języku Elixir wykonamy to przy pomocy paczki modułów `Date` i `DateTime`.
Oto przykładowy kod:

```elixir
data = Date.new(2022, 1, 1)
IO.puts Date.to_string(data)
```

Dajmy przykład z `DateTime`:

```elixir
data_i_czas = DateTime.utc_now()
IO.puts DateTime.to_string(data_i_czas)
```

Podczas wykonania powyższego kodu, wypisane zostaną aktualne data i czas w czytelnym formacie tekstowym.

## Dogłębniej

Moduły `Date` i `DateTime` są integralną częścią języka Elixir od wersji 1.3, wydanej w 2016 roku. Przed ich wprowadzeniem, konwersje dat musiały być realizowane za pomocą zewnętrznych bibliotek.

Alternatywą może być bezpośrednie formatowanie łańcuchów tekstowych, choć jest to technika zalecana raczej przy mniej skomplikowanych przypadkach, głównie ze względu na wydajność i potencjalne problemy z lokalizacją.

Pod spodem, konwersja `Date.to_string/1` i `DateTime.to_string/1` działa poprzez skonstruowanie łańcuchów znakowych reprezentujących poszczególne składowe daty (rok, miesiąc, dzień, godzina, itd.), a następnie złączenie ich w odpowiednim formacie.

## Zobacz także

1. Dokumentacja modułu `Date` w Elixirze: https://hexdocs.pm/elixir/Date.html
2. Dokumentacja modułu `DateTime` w Elixirze: https://hexdocs.pm/elixir/DateTime.html
3. Poradnik na temat formatowania łańcucha znaków w Elixirze: https://hexdocs.pm/elixir/String.html