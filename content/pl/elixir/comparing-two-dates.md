---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Porównywanie dwóch dat w języku Elixir

## Co i dlaczego?

Porównywanie dat w programowaniu polega na ustaleniu, która data jest wcześniejsza lub późniejsza. Programiści często używają tego do sortowania danych według daty i godziny oraz do kalkulacji różnicy czasu między dwoma datami.

## Jak to zrobić:

W Elixirze porównanie dwóch dat jest bardzo proste. Poniżej znajdują się przykładowe bloki kodu.

```elixir
date1 = ~D[2022-01-01]
date2 = ~D[2022-12-31]

if Date.compare(date1, date2) == :lt do
  IO.puts "date1 jest wcześniejszy"
else
  IO.puts "date2 jest późniejszy"
end
```
Gdy uruchomisz powyższy kod, zobaczysz wynik: "date1 jest wcześniejszy".

## Głębsze spojrzenie

Porównywanie dat w Elixirze, jak większość języków programowania, korzysta z api's datetime. Historycznie, programiści tworzyli własne funkcje do porównywania czasu, co było niewłaściwe i skomplikowane. Dzięki wbudowanej funkcji `Date.compare`, Elixir ułatwia tę pracę.

Innymi technikami porównywania dat są konwersje do sekund, a następnie porównanie tych wartości, ale Elixir radzi sobie z tym za nas, oferując wyraźne i łatwe do zrozumienia rozwiązanie.

Jeśli chodzi o szczegóły implementacji, `Date.compare` zwraca `:lt` (less than), `:gt` (greater than) lub `:eq` (equals), co jest typowe dla porównywania w Elixirze. Wywołujesz tylko tę funkcję, podając dwie daty jako argumenty, co czyni ją skuteczną i prostą w użyciu.

## Zobacz także

Jest wiele źródeł online dla tych, którzy chcą zgłębić temat dat i czasu w Elixirze. Oto kilka z nich:
- Oficjalna dokumentacja Elixir: https://hexdocs.pm/elixir/Date.html
- Ciekawy wpis na blogu o obsłudze czasu w Elixirze: https://dev.to/cassiozen/dates-and-time-for-elixir
- Przewodnik po DateTime w Elixirze na stronie Elixir School: https://elixirschool.com/pl/lessons/basics/date_time/