---
title:                "Porównywanie dwóch dat"
html_title:           "Elixir: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat może wydawać się prostym zadaniem, ale może okazać się przydatne w wielu przypadkach. Na przykład, gdy chcemy sprawdzić, czy dana data jest przed lub po innej, lub jeśli chcemy znaleźć różnicę między dwiema datami.

## Jak to zrobić

Porównywanie dat w Elixir jest dość proste dzięki dostępnym funkcjom i operatorom.

Wyobraźmy sobie, że chcemy sprawdzić, czy dana data jest przed inną. Możemy to zrobić za pomocą operatora ">". Przykład:

```elixir
"2020-05-24" > "2020-05-20"
# wynik: true
```

Możemy również użyć funkcji `Date.compare/2`, aby porównać dwie daty i uzyskać wartość 1, 0 lub -1 w zależności od tego, czy pierwsza data jest większa, równa lub mniejsza niż druga. Przykład:

```elixir
Date.compare(~D[2020-05-24], ~D[2020-05-20])
# wynik: 1
```

Aby znaleźć różnicę czasu między dwoma datami, możemy użyć funkcji `DateTime.diff/2`, która zwróci różnicę w sekundach. Przykład:

```elixir
DateTime.diff(~U[2020-05-20T14:00:00Z], ~U[2020-05-15T12:00:00Z])
# wynik: 432000
```

Możemy także użyć funkcji `Calendar.DateTime.diff/2`, która zwróci różnicę w postaci krotki z trzema elementami: dni, godzin i sekund. Przykład:

```elixir
Calendar.DateTime.diff(~U[2020-05-20T14:00:00Z], ~U[2020-05-15T12:00:00Z])
# wynik: {5, 2, 0}
```

## Deep Dive

Aby zagłębić się w temat porównywania dat w Elixir, warto wiedzieć, że te funkcje i operatory działają na typach danych `Date`, `DateTime` oraz `NaiveDateTime`. Istnieją również specjalne moduły dla tych typów danych, takie jak `Date` i `DateTime` w module `Calendar`.

Warto również zauważyć, że Elixir używa "Erlang Time" (ang. Erlang Time), czyli liczby sekund od 1 stycznia 1970 roku, jako podstawowego sposobu reprezentacji dat. Jednak dla wygody użytkowników, Elixir dostarcza wygodny składniowo sposób definiowania dat, jak pokazane w przykładach powyżej.

## Zobacz też

- Dokumentacja Elixir dotycząca porównywania dat: https://hexdocs.pm/elixir/DateTime.html#module-comparatons
- Wprowadzenie do operacji na datach w Elixir: https://elixirschool.com/en/lessons/basics/comparisons/