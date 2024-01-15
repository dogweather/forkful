---
title:                "Wydobywanie podciągów."
html_title:           "Elm: Wydobywanie podciągów."
simple_title:         "Wydobywanie podciągów."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że podczas pracy z tekstami, potrzebujemy wyciągnąć z nich część tekstu, która pasuje do naszych potrzeb. W takich sytuacjach przydaje się umiejętność wyciągania podciągów (ang. substring) z ciągów znaków. W tym artykule dowiesz się jak wykorzystać tę funkcjonalność w języku Elm.

## Jak to zrobić

W Elm istnieje wiele sposobów na wyciąganie podciągów, a wszystkie korzystają z funkcji `String.slice`, która przyjmuje trzy argumenty: początek, koniec oraz ciąg znaków. Oto przykład użycia tej funkcji:

```Elm
String.slice 3 6 "Ala ma kota"
```

W powyższym przykładzie, wyciągamy podciąg z ciągu znaków "Ala ma kota", zaczynając od znaku na 3 pozycji (licząc od 0) do znaku na 6 pozycji (nie włączając tego znaku). Wynikiem będzie " ma".

Możemy również podać tylko dwa argumenty, wtedy `String.slice` będzie wyciągać podciąg od początku do podanego znaku:

```Elm
String.slice 1 "Hello World"
```

W tym przypadku, zostanie zwrócony podciąg "ello World".

Funkcja `String.left` i `String.right` są przydatne, gdy chcemy wyciągnąć pewną liczbę znaków z początku lub końca ciągu.

```Elm
String.left 3 "Elm programming"
```
W wyniku otrzymamy "Elm".

## Deep Dive

Jeśli chcemy wyciągnąć podciąg z użyciem bardziej skomplikowanych warunków, możemy użyć funkcji `String.filter`. Przyjmujemy jako jej argumenty funkcję, która zwraca wartość logiczną oraz ciąg znaków, na którym będziemy operować. Na przykład, chcemy wyciągnąć tylko cyfry z ciągu znaków "Elm1Rocks":

```Elm
String.filter Char.isDigit "Elm1Rocks"
```

W wyniku otrzymamy "1".

Mamy również dostęp do funkcji `String.contains`, która sprawdza, czy dany podciąg znajduje się w ciągu znaków oraz `String.index`, która zwraca pozycję podanego znaku w ciągu. Te funkcje mogą być przydatne w bardziej zaawansowanych przypadkach wybierania podciągów.

## Zobacz również

- Oficjalna dokumentacja języka Elm: https://guide.elm-lang.org/
- Przykłady użycia funkcji `String.slice`: https://ellie-app.com/new
- Wyciąganie podciągów w Elm na przykładzie tekstu z API: https://medium.com/@thomasweiser/elm-snippet-extracting-text-from-api-responses-fc88609a7a0c