---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Elixir: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Kalkulowanie daty w przyszłości lub przeszłości jest częstym wyzwaniem dla programistów. Może to być wymagane przez różne aplikacje, takie jak aplikacje do planowania spotkań lub aplikacje do przypominania o ważnych datach. W dzisiejszym artykule dowiesz się, jak użyć Elixir, aby prostym i skutecznym sposobem przeliczać daty w przyszłości lub przeszłości.

## Jak to zrobić

Aby przeliczyć datę w przyszłości lub przeszłości w Elixirze, musimy skorzystać z biblioteki "Calendar". Ta biblioteka zawiera wiele użytecznych funkcji związanych z datami, w tym funkcję "add" służącą do dodawania lub odejmowania określonej liczby dni od aktualnej daty. Przykładowy kod wyglądałby następująco:

```elixir
require Calendar

future_date = Calendar.DateTime.add(Calendar.local_time(), 10, :days)
past_date = Calendar.DateTime.add(Calendar.local_time(), -5, :days)

IO.puts("Data w przyszłości: #{Calendar.DateTime.to_iso8601(future_date)}")
IO.puts("Data w przeszłości: #{Calendar.DateTime.to_iso8601(past_date)}")
```

Wywołajmy funkcję "add" i przekażmy jej trzy argumenty: bieżącą datę, liczbę dni do dodania lub odjęcia, oraz jednostkę czasu, w której wyrażona jest liczba dni. W powyższym przykładzie, wywołanie funkcji "Calendar.local_time()" zwraca bieżącą datę, a "to_iso8601" służy do sformatowania daty w postaci czytelnej dla człowieka. Wynikiem powyższego kodu będzie:

```
Data w przyszłości: 2021-09-07T18:00:00Z
Data w przeszłości: 2021-08-23T18:00:00Z
```

Mamy więc prosty sposób na obliczenie daty w przyszłości lub przeszłości z użyciem Elixira.

## Dogłębne zagłębianie się

W bibliotece "Calendar" istnieje wiele innych funkcji związanych z datami, takich jak "add!" do bezpośredniego modyfikowania daty lub "diff" do obliczania różnicy między dwiema datami. Możesz także użyć funkcji z biblioteki "Timex", która dostarcza jeszcze więcej opcji manipulacji datami w Elixirze.

## Zobacz także

- Dokumentacja Elixir do funkcji "Calendar.add": [https://hexdocs.pm/elixir/Calendar.html#add/3](https://hexdocs.pm/elixir/Calendar.html#add/3)
- Poradnik Elixira dotyczący manipulacji datami: [https://elixirschool.com/pl/lessons/specifics/ecto/](https://elixirschool.com/pl/lessons/specifics/ecto/)
- Strona projektu "Timex": [https://github.com/bitwalker/timex](https://github.com/bitwalker/timex)