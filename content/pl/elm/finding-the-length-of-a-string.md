---
title:                "Elm: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Poszukiwanie długości ciągu znaków (string) jest jedną z podstawowych operacji w wielu językach programowania, w tym w Elm. Znalezienie długości tekstu może być niezbędne w celu przeprowadzenia różnych operacji, na przykład podziału tekstu na mniejsze części lub sprawdzenia, czy tekst spełnia określone wymagania.

## Jak to zrobić

Aby uzyskać długość ciągu znaków (string) w Elm, można użyć funkcji `String.length`. Należy jednak pamiętać, że ta funkcja zwraca liczbę znaków w ciągu, a nie liczbę wyświetlanych znaków. Na przykład, jeśli masz ciąg znaków zawierający emotikony, które zostały zapisane jako sekwencje znaków Unicode, funkcja `String.length` zwróci liczbę sekwencji znaków, a nie faktyczną liczbę emotikonów.

Przykład:

```Elm
import String

text = "Cześć 👋🏼" -- tekście jest 7 znaków, 5 sekwencji Unicode

String.length text --> 7
```

## Głębsza analiza

Operacje na ciągach znaków w Elm są wykonywane za pomocą modułu `String`. Moduł ten oferuje wiele przydatnych funkcji, w tym `String.length`, ale również inne, takie jak `String.contains`, `String.split` czy `String.trim`. Warto zapoznać się z dokumentacją modułu `String`, aby lepiej poznać dostępne funkcje i sposoby manipulacji ciągami znaków w Elm.

## Zobacz również

- Dokumentacja modułu `String`: https://package.elm-lang.org/packages/elm/core/latest/String
- Wideo tutorial dotyczące manipulacji tekstem w Elm: https://www.youtube.com/watch?v=ROwbzrsijMA
- Przykładowy projekt z możliwością wykorzystania funkcji `String.length`: https://github.com/rtfeldman/elm-spa-example/tree/master/src/Posts