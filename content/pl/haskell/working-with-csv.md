---
title:                "Praca z plikami CSV"
html_title:           "Haskell: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, który często pracuje z danymi, to prawdopodobnie spotkałeś się z formatem CSV - Comma-Separated Values (wartości rozdzielane przecinkami). CSV jest łatwy do zrozumienia i przejrzysty, więc jest idealnym formatem do przechowywania i przesyłania danych. W tym artykule dowiesz się, jak za pomocą języka Haskell możesz łatwo pracować z plikami CSV i przetwarzać w nich dane.

## Jak

Działanie z plikami CSV w Haskell jest bardzo proste. Aby rozpocząć, musisz zainstalować pakiet ``csv``, który jest dostępny w większości menedżerów pakietów dla Haskella. Następnie, w swoim kodzie musisz dodać import ``Data.Csv`` i ``GHC.Generics``, ponieważ będziemy korzystać z uogólnionych typów danych.

1. Aby wczytać dane z pliku CSV, możesz użyć funkcji `decode` i podać jej typ danych, do którego chcesz skonwertować dane. Na przykład, jeśli masz plik CSV z danymi typu `Person`, możesz wczytać go w następujący sposób:

```Haskell
import Data.Csv
import GHC.Generics

data Person = Person {name :: String, age :: Int} deriving (Generic, Show)
instance FromNamedRecord Person

main :: IO ()
main = do
    csv <- BL.readFile "people.csv"
    case decodeByName csv of
        Left err -> putStrLn err
        Right (_, v) -> V.mapM_ print (v :: Vector Person)
```

2. Aby zapisać dane do pliku CSV, możesz skorzystać z funkcji `encode` i podać jej listę wartości typu ``ToNamedRecord``. Na przykład, jeśli chcesz zapisać dane `Person` do pliku CSV, możesz to zrobić w ten sposób:

```Haskell
import Data.Csv
import GHC.Generics

data Person = Person {name :: String, age :: Int} deriving (Generic, Show)
instance ToNamedRecord Person

main :: IO ()
main = do
    let people = [Person "Anna" 25, Person "Bartek" 30, Person "Kasia" 28]
    BL.writeFile "people.csv" (encodeDefaultOrderedByName people)
```

3. Aby przetwarzać dane z plików CSV, możesz posłużyć się funkcjami takimi jak `lookup` lub `map`, które pozwalają na łatwe operacje na kolumnach i wierszach pliku CSV. Na przykład, jeśli chcesz wyszukać tylko osoby powyżej 30 roku życia, możesz użyć funkcji `filter`:

```Haskell
import Data.Csv
import GHC.Generics

data Person = Person {name :: String, age :: Int} deriving (Generic, Show)
instance FromNamedRecord Person

main :: IO ()
main = do
    csv <- BL.readFile "people.csv"
    case decodeByName csv of
        Left err -> putStrLn err
        Right (_, v) -> V.mapM_ print (V.filter (\p -> age p > 30) v :: Vector Person)
```

## Deep Dive

Głównym typem danych w pakiecie ``csv`` jest `NamedCsv`, który składa się z ``Header`` i ``Record``. Struktura `Header` zawiera nazwy kolumn, a `Record` to pojedynczy wiersz danych. Ponadto, możesz wykorzystać funkcję `parseNamedCsv` do własnej implementacji przetwarzania plików CSV.

## Zobacz również

- [Dokumentacja pakietu CSV dla Haskella](https://hackage.haskell.org/package/csv)
- [Przykładowy kod dla pracowania z CSV w Haskelli](https://github.com/andrewsharmon/csv-haskell)