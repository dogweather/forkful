---
title:                "Wyszukiwanie i zamiana tekstu"
html_title:           "Elm: Wyszukiwanie i zamiana tekstu"
simple_title:         "Wyszukiwanie i zamiana tekstu"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Szukanie i zamiana tekstu to podstawowy element programowania. Jest to codzienna czynność dla każdego, kto pracuje z tekstem. W tym artykule opowiemy o tym, jak wykorzystać język Elm do automatyzacji tego procesu i zaoszczędzić czas.

## Jak to zrobić

Za pomocą funkcji `replace` możesz łatwo zmienić wybrany tekst na inny. Poniżej znajduje się przykładowy kod wykorzystujący tę funkcję:
```Elm
replace "Hello World!" "Hello" "Hi"
```
W powyższym przykładzie, tekst "Hello World!" zostanie zastąpiony przez "Hi World!". Funkcja `replace` przyjmuje trzy argumenty: tekst, który chcemy zmienić, tekst, który chcemy usunąć oraz tekst, którym chcemy go zastąpić.

Możesz również użyć funkcji `replaceAll` do zmiany wszystkich wystąpień danego tekstu na inny. Poniżej znajduje się kod wykorzystujący tę funkcję:
```Elm
replaceAll "aaabbbccc" "a" "x"
```
W tym wypadku, tekst "aaabbbccc" zostanie zmieniony na "xxxbbbccc".

Każda z funkcji `replace` i `replaceAll` zwraca zmieniony tekst jako wynik. Możesz go przypisać do zmiennej i wykorzystać w dalszej części kodu.

## Głębsze wgląd

W języku Elm istnieje wiele innych funkcji związanych ze zmianą tekstu, takich jak `removeFirst`, `removeLast`, `trim`, `split`, `join` i wiele innych. Możesz dowiedzieć się więcej o nich w [oficjalnej dokumentacji Elm](https://guide.elm-lang.org/strings/).

Jedną z przydatnych funkcji jest również `replaceChar`, która pozwala na zmianę pojedynczego znaku w tekście. Na przykład, można zmienić wszystkie małe litery w tekście na duże lub na odwrót.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o tworzeniu aplikacji w języku Elm, polecamy zapoznać się z [oferowanymi przez nich kursami](https://www.elm-in-action.com/) lub zasubskrybować [podręcznik "Elm in Action"](https://www.manning.com/books/elm-in-action).