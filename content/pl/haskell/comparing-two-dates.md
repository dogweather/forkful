---
title:                "Haskell: Porównywanie dwóch dat"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest częstym problemem w programowaniu. Może to być przydatne w różnych aplikacjach, takich jak systemy rezerwacji, planowanie wydarzeń, czy generowanie raportów. W języku Haskell mamy dostęp do wielu narzędzi, które ułatwiają nam to zadanie.

## Jak to zrobić

W Haskellu porównywanie dwóch dat jest bardzo proste. Najpierw musimy skonwertować daty na odpowiednią strukturę danych, na przykład ```Day```, a następnie wykorzystać funkcję ```compare```. Przykładowy kod wyglądałby mniej więcej tak:

```Haskell
import Data.Time

-- Definiujemy dwie daty
firstDate = fromGregorian 2020 10 15
secondDate = fromGregorian 2020 9 10

-- Porównujemy dwie daty
result = compare firstDate secondDate

-- Wypisujemy rezultat porównania
putStrLn (show result)
```

Po wykonaniu tego kodu, powinniśmy zobaczyć w konsoli wartość ```GT```, co oznacza, że pierwsza data jest późniejsza niż druga. Ponadto, funkcja ```compare``` może zwrócić również wartości ```LT```, czyli pierwsza data jest wcześniejsza, oraz ```EQ```, gdy obie daty są identyczne.

## Głębszy zanurzenie

Podczas porównywania dwóch dat w Haskellu, warto zwrócić uwagę na pewne szczegóły. Po pierwsze, funkcja ```compare``` jest bardzo ogólna i może porównać nie tylko daty, ale również inne typy danych, na przykład liczby czy napisy. Dlatego należy pamiętać o konwersji naszych dat na odpowiedni typ.

Po drugie, warto zwrócić uwagę na to, że w języku Haskell daty są traktowane jako obiekty niemutowalne, czyli nie można zmienić wartości już wcześniej zdefiniowanej daty. W przypadku, gdy potrzebujemy porównać dwie daty i jedna z nich została zdefiniowana wcześniej, musimy wykorzystać odpowiednią funkcję, na przykład ```addDays```, aby dodać do niej określoną liczbę dni i otrzymać nowy obiekt daty.

## Zobacz także

- [Dokumentacja języka Haskell](https://www.haskell.org/documentation/)
- [Porównywanie typów danych w Haskellu](https://wiki.haskell.org/Type_compatibility_and_equality)
- [Przykładowa implementacja porównywania dat w Haskellu](https://rosettacode.org/wiki/Compare_dates)