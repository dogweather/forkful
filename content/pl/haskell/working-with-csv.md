---
title:                "Praca z plikami csv"
html_title:           "Haskell: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/working-with-csv.md"
---

{{< edit_this_page >}}

## Co to jest CSV i dlaczego programiści z tym pracują?

CSV to format pliku, w którym dane są przechowywane w postaci tabeli, z kolumnami i wierszami. W programowaniu często pracujemy z CSV, ponieważ jest to wygodny sposób na przechowywanie i przetwarzanie danych w programach.

## Jak to zrobić:

```Haskell
import Text.CSV

-- Wczytanie pliku CSV
inputFile :: String
inputFile = "dane.csv"

main :: IO ()
main = do
    file <- readFile inputFile
    let data = lines file
        parsedCsv = parseCSV inputFile data
    case parsedCsv of
        Left err -> putStrLn "Wystąpił błąd podczas parsowania pliku CSV"
        Right result -> mapM_ print result -- Wyświetlanie zawartości pliku CSV

-- Dodanie wiersza do istniejącego pliku CSV
outputFile :: String
outputFile = "wynik.csv"

main :: IO ()
main = do
    appendFile outputFile "Nowy wiersz,nowa,kolumna"
```

## Głębsza analiza:

Format CSV został stworzony w celu przechowywania danych tabelarycznych w plikach tekstowych, aby ułatwić współpracę między różnymi systemami komputerowymi. Alternatywami dla CSV są między innymi formanty XML i JSON, ale CSV jest nadal często używany, ponieważ jest prostszy i zajmuje mniej miejsca.

W bibliotece `text.CSV` w języku Haskell znajdują się funkcje, które pozwalają na łatwe wczytywanie i przetwarzanie plików CSV. Używając funkcji `parseCSV`, możemy sparsować plik CSV i wyświetlić jego zawartość. Aby dodawać wiersze do istniejącego pliku CSV, możemy użyć funkcji `appendFile`.

## Zobacz także:

- [Dokumentacja biblioteki `text.CSV` w Hackage](https://hackage.haskell.org/package/csv)