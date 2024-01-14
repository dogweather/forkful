---
title:                "Haskell: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

CSV, czyli format plików zawierających dane tabelaryczne, jest powszechnie wykorzystywany w programowaniu. Jest to niezwykle przydatny sposób przechowywania i przetwarzania informacji. W tym wpisie dowiecie się, dlaczego warto poznać technikę pracy z plikami CSV w języku Haskell.

## Jak to zrobić

Kodowanie plików CSV w języku Haskell jest wyjątkowo proste i intuicyjne dzięki bibliotece Data.CSV. Wystarczy ją zainstalować przy użyciu menedżera pakietów Cabal lub Stack, a następnie zaimportować w swoim projekcie.

```Haskell
import Text.CSV
```

Aby wczytać plik CSV, używamy funkcji `parseCSVFromFile` z argumentem będącym nazwą pliku. Funkcja zwraca wartość typu `Either ParseError CSV`, co oznacza, że może zwrócić błąd lub obiekt typu CSV.

```Haskell
csv <- parseCSVFromFile "dane.csv"
```

Aby przetworzyć dane z pliku CSV, możemy skorzystać z wygodnego konstruktora `Record` lub funkcji `fromRecord` do zamiany wierszy na listy z wartościami.

```Haskell
data = fromRecord $ Record ["Haskell", "jest", "fajny"]
```

## Głębsze zanurzenie

Istnieje wiele możliwości przetwarzania i filtrowania danych zawartych w plikach CSV, na przykład za pomocą funkcji `filter`, `map` lub `foldr`. Funkcja `filter` pozwala na wybranie tylko tych wierszy spełniających określone warunki, `map` służy do modyfikacji poszczególnych elementów w wierszach, a `foldr` pozwala na agregację danych.

```Haskell
filter (\row -> row !! 1 == "Haskell") csv -- wybierz wiersze, w których druga kolumna zawiera słowo "Haskell"
map (\row -> row !! 0) csv -- zwróc listę wartości z pierwszej kolumny
foldr (\row acc -> if read (row !! 2) > acc then read (row !! 2) else acc) 0 csv -- znajdź największą wartość w trzeciej kolumnie
```

## Zobacz także

- [Dokumentacja biblioteki Data.CSV](https://hackage.haskell.org/package/csv)
- [Tutorial pracy z plikami CSV w języku Haskell](https://markkarpov.com/tutorial/a-tutorial-on-a-practical-haskell-unicode-library-csv.html)
- [Przykładowe projekty wykorzystujące pliki CSV w Haskellu](https://www.reddit.com/r/haskell/comments/6n3p0j/examples_of_useful_csvrearwing_projects/)