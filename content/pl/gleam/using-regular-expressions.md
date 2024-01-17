---
title:                "Używanie wyrażeń regularnych"
html_title:           "Gleam: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i Dla Czego?

Używanie wyrażeń regularnych (regular expressions) jest powszechną praktyką wśród programistów. Jest to sposób na wyszukiwanie, zastępowanie i manipulację tekstem zgodnie z określonymi wzorcami. Dzięki temu, programiści mogą szybko i dokładnie odnaleźć potrzebne im informacje w długich i skomplikowanych plikach tekstowych.

## Jak To Zrobić?

Gleam oferuje bogaty zestaw funkcji do obsługi wyrażeń regularnych. Przykładowo, jeśli chcemy odnaleźć wszystkie liczby całkowite w ciągu znaków, możemy użyć funkcji ```Regex.matches``` w ten sposób:

```Gleam
Regex.matches("[0-9]+", "123 abc 456")
```

Output: ["123", "456"]

Możemy również użyć wildcardów (znak przypominający wielokropek) do dopasowania dowolnego znaku lub ciągu znaków. Na przykład, jeśli chcemy znaleźć wszystkie wyrazy zaczynające się na "a" i kończące się na "b", możemy użyć wyrażenia ```a*b```.

## Zagłębianie Się W Temat

Wyrażenia regularne mają długą historię i są obecne w wielu językach programowania. Jednym z popularnych zastosowań jest walidacja formularzy na stronach internetowych. Istnieją również inne metody manipulacji tekstem, takie jak użycie parserów lub bibliotek do parsowania HTML.

Podczas implementacji wyrażeń regularnych, ważne jest zapoznanie się z dostępnymi regexowymi operatorami i składnią. Można także skorzystać z istniejących bibliotek, jeśli potrzebujemy bardziej złożonych funkcji.

## Zobacz Również

- Oficjalna dokumentacja Gleam: https://gleam.run/stdlib/regex.html
- Poradnik na temat wyrażeń regularnych w języku angielskim: https://www.regular-expressions.info/