---
title:                "Elixir: Zmiana wielkości litery w ciągu znaków"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego?

Często w programowaniu napotykamy sytuacje, w których potrzebujemy zmodyfikować tekst, np. zmieniając pierwszą literę na wielką. W języku Elixir możemy skorzystać z funkcji `String.capitalize/1`, która umożliwia właśnie to. W tym artykule dowiesz się, dlaczego warto używać tej funkcji oraz jak jej używać.

## Jak użyć funkcji `String.capitalize/1`?

Aby skorzystać z funkcji `String.capitalize/1`, musimy przekazać jej jako argument napis, który chcemy zmodyfikować. Możemy to zrobić w następujący sposób:

```elixir
String.capitalize("elixir")
```

Po uruchomieniu powyższego kodu otrzymamy wynik:

```elixir
"Elixir"
```

Jak widzimy, funkcja `String.capitalize/1` zmieniła pierwszą literę w napisie "elixir" na wielką.

Możemy również przekazać do funkcji znak specjalny, który zostanie zignorowany, jak na przykład spacja:

```elixir
String.capitalize("elixir rocks")
```

Wynik:

```elixir
"Elixir rocks"
```

W razie potrzeby, możemy przekazać do funkcji napis zawierający wiele wyrazów i funkcja zmieni pierwsze litery wszystkich tych wyrazów:

```elixir
String.capitalize("elixir programming language")
```

Wynik:

```elixir
"Elixir Programming Language"
```

Warto również zauważyć, że funkcja `String.capitalize/1` nie zmieni już występujących wielkich liter, jedynie pierwszą literę, dlatego nie musimy martwić się o to, czy napis jest już w jakiś sposób zmodyfikowany.

## Głębsza analiza `String.capitalize/1`

Funkcja `String.capitalize/1` korzysta z funkcji `String.upcase/1` oraz `String.downcase/1`, aby zmienić pierwszą literę napisu na wielką. Jest to zatem bardzo wygodne i efektywne rozwiązanie, które możemy wykorzystywać w naszym kodzie.

Ponadto, funkcja ta jest niezwykle przydatna w sytuacjach, gdy musimy zapewnić jednolity format tekstu, na przykład w formularzach lub wyświetlanych danych.

## Zobacz również

- Dokumentacja funkcji [`String.capitalize/1`](https://hexdocs.pm/elixir/String.html#capitalize/1)
- Wideo tutorial na temat manipulacji tekstami w Elixir (w języku angielskim) [Link](https://www.youtube.com/watch?v=y66Q7uT2Jo4)
- Przewodnik na temat pracy z napisami w Elixir (w języku angielskim) [Link](https://dev.to/cassvanr/handling-strings-in-elixir-easily-1e9a)