---
title:    "Elm: Porównywanie dwóch dat"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Dlaczego?

Porównywanie dwóch dat może być przydatne w wielu przypadkach programowania. Może pomóc w sprawdzaniu, czy data jest przed lub po określonej dacie, porównaniu bieżącej daty z zapisaną w bazie danych lub weryfikacji poprawności wprowadzonej przez użytkownika daty. W tym poście dowiesz się, jak porównać dwie daty w języku Elm i jak wykorzystać tę funkcję w swoim kodzie.

## Jak to zrobić?

Aby porównać dwie daty w Elm, musimy użyć modułu `Date` dostępnego w standardowej bibliotece. W tym przykładzie użyjemy funkcji `compare`, która przyjmuje dwa argumenty typu `Date` i zwraca wartość `LT` (mniejsza), `EQ` (równa) lub `GT` (większa) w zależności od wyniku porównania.

```Elm
import Date exposing (Date, compare)

date1 : Date
date1 = Date.fromString "2021-01-01"

date2 : Date
date2 = Date.fromString "2021-05-23"

result : Date.Order
result = compare date1 date2

-- Output: GT
```

W powyższym przykładzie porównujemy dwie daty z użyciem funkcji `compare` i przypisujemy wynik do zmiennej `result`. Następnie, wyświetlamy tę zmienną, aby zobaczyć wynik porównania. Możemy również porównać daty za pomocą operatorów logicznych `==` (równe) i `/=` (różne).

## Głębszy zanurzenie

Aby lepiej zrozumieć jak działa porównywanie dat w Elm, warto wiedzieć jak wartości są przechowywane i porównywane w tym języku. W Elm, daty są reprezentowane przez typ `Date`, który składa się z wartości numerycznych dla roku, miesiąca i dnia. Kiedy porównujemy dwie daty, porównywane są te wartości numeryczne w kolejności od najważniejszej (rok) do najmniej ważnej (dzień).

Ponadto, warto pamiętać, że porównanie dat jest dokładne tylko do dnia. Oznacza to, że jeśli porównujemy daty z różnymi godzinami, minutami lub sekundami, wynik może być nieprzewidywalny.

## Zobacz również

* Dokumentacja modułu `Date` w języku Elm: https://package.elm-lang.org/packages/elm/time/latest/Date
* Inne przydatne funkcje związańne z datami w Elm: https://dev.to/jkachuck/working-with-dates-in-elm-5jio