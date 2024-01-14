---
title:                "Elm: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą języka Elm, na pewno zetknąłeś się z problemem porównywania dwóch dat. Może to być wyzwanie dla wielu osób, więc dziś przygotowaliśmy dla Ciebie krótki tutorial, który pomoże Ci poradzić sobie z tym zadaniem.

## Jak to zrobić

Porównywanie dwóch dat w Elm może być łatwe, jeśli użyjesz odpowiednich narzędzi. Jednym z najprostszych sposobów jest użycie funkcji `compare` z modułu `Date`, która zwróci wartość `LT` (less than), `EQ` (equal) lub `GT` (greater than) w zależności od porównywanych dat.

```Elm
zmienna1 = Date.fromString "2021-01-01"
zmienna2 = Date.fromString "2021-02-01"
comparacja = Date.compare zmienna1 zmienna2

-- zmienna1 jest mniejsza niż zmienna2, więc comparacja będzie zawierać wartość LT
```

Możesz również użyć funkcji `isBefore`, `isAfter` lub `isSame` zamiast `compare`, aby sprawdzić, czy jedna data jest przed, po lub równa drugiej.

```Elm
zmienna1 = Date.fromString "2021-01-01"
zmienna2 = Date.fromString "2021-02-01"
isBefore = Date.isBefore zmienna1 zmienna2

-- zmienna1 jest przed zmienna2, więc isBefore będzie zawierać wartość True
```

## Głębsza analiza

Porównywanie dat może być trudne, jeśli nie wiesz, jak przetwarzać różne formaty dat. W Elm możesz użyć funkcji `fromCalendarDate` lub `fromIsoString`, aby przekonwertować datę do odpowiedniego formatu.

```Elm
formatDate = Date.fromCalendarDate 2021 1 1
formatIsoDate = Date.fromIsoString "2021-01-01"

-- obie funkcje zwrócą tę samą datę w różnych formatach, dlatego ważne jest wybranie odpowiedniego dla Twoich potrzeb
```

Pamiętaj również o tym, że porównywanie dat może być problematyczne, jeśli Twoja aplikacja jest używana w różnych strefach czasowych. W takiej sytuacji warto skorzystać z modułu `Time`, który umożliwia obliczanie różnicy czasu między datami w różnych strefach.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o porównywaniu dat w języku Elm, polecamy Ci zapoznać się z dokumentacją na temat modułu `Date` i `Time`.

1. https://package.elm-lang.org/packages/elm/time/latest/
2. https://package.elm-lang.org/packages/elm/core/latest/Date

Mamy nadzieję, że ten krótki tutorial pomógł Ci lepiej zrozumieć, jak porównywać daty w języku Elm. W razie jakichkolwiek pytań, zachęcamy do zadawania ich w komentarzach poniżej. Dziękujemy za przeczytanie!