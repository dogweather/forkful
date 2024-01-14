---
title:    "Elixir: Porównywanie dwóch dat"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Czas jest jednym z najważniejszych elementów w każdym języku programowania, ponieważ wiele aplikacji korzysta z daty i czasu do wykonywania różnych operacji. Dlatego porównywanie dwóch różnych dat może być bardzo pomocne w wielu scenariuszach. W tym artykule dowiesz się, jak to zrobić w Elixirze.

## Jak to zrobić

Aby porównać dwie daty w Elixirze, musimy najpierw skonwertować je na typ Date. Można to zrobić za pomocą funkcji ```Elixir Date.from_iso8601("YYYY-MM-DD")```. Następnie możemy użyć operatorów porównania, takich jak ```<```, ```>```, ```<=```, ```>=``` do porównania dwóch dat.

Przykład:

```Elixir
first_date = Date.from_iso8601("2021-01-01")
second_date = Date.from_iso8601("2021-02-15")

IO.puts(first_date < second_date) # true
```

Możemy również użyć funkcji ```Elixir Date.compare/2```, aby otrzymać wynik porównania jako liczba całkowita, co jest przydatne, gdy chcemy porównywać daty z różnych stref czasowych.

Przykład:

```Elixir
first_date = Date.from_iso8601("2020-12-01")
second_date = Date.from_iso8601("2021-01-15")

IO.puts(Date.compare(first_date, second_date)) # -1 (pierwsza data jest wcześniejsza)
```

## Deep Dive

W Elixirze, porównywanie dwóch dat jest możliwe dzięki zaimplementowaniu protokołu ```Elixir Comparable``` przez typ ```Elixir Date```. Dzięki temu możemy używać operatorów porównania, takich jak ```<```, ```>```, itp., ale także funkcji takich jak ```Elixir Date.min/2``` i ```Elixir Date.max/2```, aby znaleźć odpowiednio najwcześniejszą lub najpóźniejszą datę.

Kiedy porównujemy dwie daty w Elixirze, porównujemy tylko ich daty, a nie również godziny i minuty. Jeśli chcesz porównać daty z godzinami, musisz użyć typu ```Elixir DateTime```.

## Zobacz także

- Dokumentacja Elixir Date: https://hexdocs.pm/elixir/Date.html
- Porównywanie wartości w Elixirze: https://elixirschool.com/pl/lessons/basics/comparisons/
- Porównywanie dat w innych językach programowania: https://www.techiedelight.com/compare-dates-in-programming-languages/