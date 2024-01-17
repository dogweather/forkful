---
title:                "Porównywanie dwóch dat"
html_title:           "Elm: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Porównywanie dwóch dat jest niezbędną częścią programowania, ponieważ umożliwia nam sprawdzenie, czy jedna data jest wcześniejsza, późniejsza czy równa drugiej. Jest to szczególnie ważne w aplikacjach, które manipulują czasami i datami, takich jak kalendarze i aplikacje finansowe.

## Jak to zrobić:

Można użyć funkcji `Date.compare` w Elm, która przyjmuje dwa argumenty typu `Date` i zwraca jedną z trzech wartości: `-1`, `0` lub `1`. Wartości te wskazują odpowiednio, czy pierwsza data jest wcześniejsza, równa lub późniejsza od drugiej. 

```Elm
import Date exposing (..)

Date.compare (fromString "2021-02-20") (fromString "2021-02-21") -- zwraca -1
```

W przypadku, gdy porównywane daty są sobie równe, funkcja zwróci wartość `0`.

Można również użyć funkcji `Date.isAfter` i `Date.isBefore`, które zwracają wartość logiczną (`True` lub `False`) i sprawdzają, czy pierwsza data jest odpowiednio późniejsza lub wcześniejsza od drugiej.

```Elm
import Date exposing (..)

Date.isAfter (fromString "2021-03-02") (fromString "2021-03-01") -- zwraca True
```

## Dogłębnie:
Funkcje porównujące daty w Elm są oparte na standardzie ISO 8601, który określa format daty jako `YYYY-MM-DD`. Wcześniej funkcje te były dostępne w module `Time`, ale zostały przeniesione do modułu `Date` w celu ustandaryzowania i ułatwienia korzystania z nich.

Jeśli chodzi o alternatywy, można również porównywać daty wykorzystując mechanizmy dostępne w JavaScript, ale wymaga to nieco więcej kodu i nie jest tak wygodne jak w przypadku Elm.

Szczegółowe informacje o tym jak funkcje porównujące daty są implementowane w Elm można znaleźć w dokumentacji języka.

## Zobacz także:
- Dokumentacja `Date` w Elm: https://package.elm-lang.org/packages/elm/time/latest/Date
- Standard ISO 8601: https://en.wikipedia.org/wiki/ISO_8601