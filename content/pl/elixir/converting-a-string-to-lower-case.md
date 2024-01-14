---
title:                "Elixir: Konwertowanie ciągu znaków na małe litery"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami, gdy pracujemy z różnymi danymi w Elixir, musimy zmienić wielkość liter w ciągu znaków. Jest to szczególnie przydatne przy walidacji danych lub porównywaniu ich wartości. W tym artykule omówimy, jak prosto i skutecznie przekształcić ciąg znaków na małe litery przy użyciu Elixir.

## Jak to zrobić

Aby przekształcić ciąg znaków na małe litery w Elixir, możemy użyć funkcji `String.downcase/1`. Przykładowe użycie wyglądałoby następująco:

```Elixir
iex> String.downcase("PRZYKLADOWY CIAG ZNAKOW")
"przykladowy ciag znakow"
```
Możemy także wykorzystać tę funkcję na listach znaków, stosując ją jako drugi argument funkcji `Enum.map/2`, na przykład:

```Elixir
iex> "PRZYKLADOWY CIAG ZNAKOW" |> String.graphemes() |> Enum.map(&String.downcase/1) |> Enum.join()
"przykladowy ciag znakow"
```
Zauważ, że używamy funkcji `String.graphemes/1` aby podzielić ciąg znaków na pojedyncze znaki, ponieważ funkcja `String.downcase/1` działa tylko na pojedyncze znaki lub listy znaków.

## Dogłębna analiza

Istnieje także funkcja `String.downcase/2`, która pozwala na określenie języka, w którym ma być przeprowadzona konwersja na małe litery. Jest to szczególnie ważne, ponieważ nie wszystkie języki mają takie same reguły przy zmienianiu wielkości liter. Na przykład, w języku tureckim znak "I" jest zmieniany na "ı" zamiast na "i" w języku angielskim. Przykładowe użycie wyglądałoby następująco:

```Elixir
iex> String.downcase("BÜYÜK I", :turkish)
"büyüki"
```

Ponadto, w celu zapewnienia całkowitej zgodności z Unicode, możemy użyć funkcji `String.downcase/3`, która przyjmuje opcjonalny argument "case" jako drugi argument i pozwala na określenie reguł dla konkretnych języków i przypadków. Przykładowe użycie wyglądałoby następująco:

```Elixir
iex> String.downcase("İ", [:case, :fold])
"i"
```

## Zobacz także

- Elixir String moduł dokumentacja: https://hexdocs.pm/elixir/String.html
- Elixir Enum moduł dokumentacja: https://hexdocs.pm/elixir/Enum.html
- Unicode Case Folding: https://unicode-table.com/en/sections/case-folding/