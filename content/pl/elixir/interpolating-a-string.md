---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolacja Stringów w Elixir

## Co i Dlaczego?

Interpolacja stringów to proces modyfikowania stringów poprzez wstawianie wyrażeń, które są dynamicznie przekształcane na tekst. Programiści używają tego, aby efektywnie tworzyć i manipulować stringami, co jest przydatne np. przy logowaniu danych.

## Jak to zrobić:

W Elixir możliwe jest stosowanie wyrażeń wewnątrz ciągów, dzięki czemu tworzenie dynamicznych stringów jest łatwe. Wystarczy umieścić wyrażenie w nawiasach klamrowych `{}` i poprzedzić go symbolem `#`.

```elixir
imie = "Kamil"
IO.puts "Cześć, #{imie}!"
```

Gdy uruchomisz powyższy kod, otrzymasz komunikat:

```
Cześć, Kamil!
```

## Deep Dive

Interpolacja ciągów znaków ma swoje korzenie w językach takich jak Perl i Ruby. Istotne jest, że Elixir, będąc językiem dynamicznym, wykonuje interpolację w czasie wykonania.

W Elixir jest wiele sposobów na konkatenację ciągów znaków, ale interpolacja jest jedną z najprostszych i najbardziej wydajnych opcji.

Pod spodem, interpolacja w Elixir jest zaimplementowana przez jądro języka, które tworzy listę binarną. Jest to efektywniejsze niż konkatenacja stringów, która wymaga kopiowania i łączenia danych.

## Zobacz także:

1. [Documentacja Elixir](https://elixir-lang.org/getting-started/basic-types.html#strings): Sprawdź oficjalną dokumentację Elixir na temat stringów i manipulacji danymi.
2. [Przewodnik HexDocs](https://hexdocs.pm/elixir/String.html): Szczegółowy przewodnik po modułach String w Elixir.
3. [StackOverflow](https://stackoverflow.com/questions/31384963/string-interpolation-vs-concatenation/): Dyskusja na temat porównania interpolacji stringów i złączeń w Elixir.