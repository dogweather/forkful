---
title:                "Pobieranie bieżącej daty"
html_title:           "Elixir: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktokolwiek chciałby poznać aktualną datę? Cóż, w zależności od potrzeb, może to być przydatne przy tworzeniu aplikacji do zarządzania zadaniem, monitoringu czasu pracy lub po prostu do wyświetlania bieżącej daty w aplikacji. W tym artykule dowiesz się, jak łatwo uzyskać aktualną datę w Elixir.

## Jak to zrobić

W Elixir, aby uzyskać aktualną datę, możesz wykorzystać funkcję `Date.utc_today`, która zwraca bieżącą datę w formacie `{{rok, miesiąc, dzień}, {godzina, minuta, sekunda}}`. Oto przykład kodu:

```
Elixir iex> Date.utc_today
{{2021, 8, 12}, {0, 0, 0}}
```

Jeśli chcesz uzyskać tylko datę bez uwzględnienia czasu, możesz użyć funkcji `Date.today`:

```
Elixir iex> Date.today
{2021, 8, 12}
```

Aby uzyskać datę w danym strefie czasowej, możesz użyć funkcji `DateTime.utc_now` lub `DateTime.now`. Na przykład:

```
Elixir iex> DateTime.utc_now("Europe/Warsaw")
~U[2021-08-12 13:15:25.726889Z]
```

Warto również wspomnieć, że istnieje wiele innych wbudowanych funkcji w Elixir, które umożliwiają manipulowanie, porównywanie i formatowanie danych daty i czasu. Więcej informacji na ten temat znajdziesz w sekcji "Deep Dive".

## Deep Dive

Elixir zawiera wiele modułów związanych z zarządzaniem datami i czasem, takich jak `Date`, `DateTime`, `Time` i `NaiveDateTime`. Te moduły oferują szeroki zakres funkcji do manipulacji i formatowania dat i czasów, w tym obliczenia różnic między datami, porównywanie dat, wyświetlanie dat w różnych strefach czasowych i wiele więcej.

Warto również wspomnieć o niesamowitej bibliotece `Calendar`, która oferuje jeszcze więcej funkcjonalności związanych z datami i czasem, takich jak wyświetlanie kalendarza, obliczanie czasu do ważnej daty i obsługa różnych kalendarzy (np. kalendarz bizantyjski czy hinduski). Dokumentacja do tej biblioteki jest bardzo obszerna i warto ją przejrzeć, aby poznać wszystkie jej możliwości.

## Zobacz także

- Dokumentacja Elixir: https://hexdocs.pm/elixir/master/Date.html
- Dokumentacja modułu Date: https://hexdocs.pm/elixir/Date.html
- Dokumentacja biblioteki Calendar: https://hexdocs.pm/calendar/readme.html