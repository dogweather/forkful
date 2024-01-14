---
title:    "Haskell: Znajdowanie długości ciągu znaków"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub chcesz zacząć swoją przygodę z programowaniem, zapewne słyszałeś o języku Haskell. Jest to funkcyjny język programowania, znany z wielu zalet, takich jak bezpieczeństwo typów i obsługa błędów. Jedną z funkcji, z których możesz skorzystać w Haskellu, jest znajdowanie długości ciągu znaków. W tym wpisie dowiesz się, dlaczego jest to przydatne i jak to zrobić.

## Jak to zrobić?

Aby znaleźć długość ciągu znaków w Haskellu, musimy użyć funkcji `length`, która jest wbudowana w ten język. Spójrzmy na prosty przykład:

```Haskell
length "Haskell"
```

To polecenie zwróci wartość `7`, ponieważ jest tyle znaków w słowie "Haskell". Możemy również użyć zmiennej, aby zapisać długość ciągu znaków i wyświetlić ją w konsoli:

```Haskell
let word = "Haskell"
length word
```

W tym przypadku otrzymamy również wartość `7`.

## Głębszy wgląd

W języku Haskell, funkcja `length` jest zdefiniowana jako:

```Haskell
length :: [a] -> Int
```

Oznacza to, że funkcja ta przyjmuje jako argument listę dowolnego typu (oznaczona jako `a`) i zwraca wartość typu `Int`. Oznacza to również, że możemy używać tej funkcji nie tylko do ciągów znaków, ale również do dowolnych list, nawet tych zawierających różne typy danych.

Ponadto, funkcja `length` jest rekurencyjna, co oznacza, że wewnętrznie wywołuje samą siebie w celu obliczenia długości listy. Dzięki temu jest bardzo wydajna i może radzić sobie z dużymi listami w krótkim czasie.

## Zobacz również

- [Dokumentacja funkcji `length` w języku Haskell](https://hackage.haskell.org/package/base-4.15.1.0/docs/Data-List.html#v:length)
- [Pętle rekurencyjne w języku Haskell](https://www.tutorialspoint.com/recursive-loops-in-haskell)

Dziękujemy za przeczytanie tego wpisu! Mamy nadzieję, że pomógł Ci zrozumieć, dlaczego i jak obliczać długość ciągu znaków w języku Haskell. Jeśli chcesz dowiedzieć się więcej o tym języku, zapraszamy do sprawdzenia innych naszych wpisów na tej stronie. Powodzenia w nauce Haskella!