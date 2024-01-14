---
title:                "Elm: Porównywanie dwóch dat"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego porównywać dwie daty?

Porównywanie dat jest ważnym elementem wielu aplikacji internetowych i programów. Często musimy sprawdzać, która data jest wcześniejsza lub późniejsza lub też czy dwie daty są sobie równe. W tym artykule dowiesz się, jak w łatwy sposób porównywać dwie daty w języku Elm.

## Jak to zrobić?

Porównywanie dwóch dat w Elm jest bardzo proste dzięki wbudowanej funkcji `compare`. Poniżej znajdziesz przykładowy kod, który porównuje dwie daty i zwraca wartość `LT`, `EQ` lub `GT` w zależności od wyniku porównania.

```Elm
date1 = Date.fromString "2021-10-15"
date2 = Date.fromString "2021-11-01"

result = Date.compare date1 date2
```

Wynik będzie równy `LT`, ponieważ data `date1` jest wcześniejsza od `date2`. Możesz także zmienić wartości dat i sprawdzić, jak zmienia się wynik porównania.

## Głębszy zanurzenie

Funkcja `compare` w języku Elm porównuje daty na podstawie ich rosnącego porządku. Oznacza to, że jeśli pierwsza data jest wcześniejsza od drugiej, to wykonywane jest porównanie według roku, miesiąca, dnia i godziny. Jeśli któreś z tych elementów jest równe, to następuje porównanie według kolejnych elementów. Jeśli wszystkie elementy są równe, to porównanie kończy się remisem i zwracana jest wartość `EQ`.

Możesz też porównywać daty w różnych strefach czasowych, używając funkcji `toTime`. Jest to przydatne, gdy pracujesz z datami pochodzącymi z różnych części świata.

## Zobacz także

1. Dokumentacja języka Elm o porównywaniu dat: https://package.elm-lang.org/packages/elm/time/latest/Time#compare
2. Przykładowe zastosowania porównywania dat: https://dev.to/johnomarkid/comparing-dates-in-javascript-and-elm-3egh
3. Porównywanie dat w języku JavaScript: https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date