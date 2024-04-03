---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:51.022036-07:00
description: "Jak to zrobi\u0107: W Haskellu obs\u0142ug\u0119 plik\xF3w CSV mo\u017C\
  na osi\u0105gn\u0105\u0107 za pomoc\u0105 biblioteki `cassava`, jednej z popularnych\
  \ bibliotek stron trzecich do tego celu.\u2026"
lastmod: '2024-03-13T22:44:35.476175-06:00'
model: gpt-4-0125-preview
summary: "W Haskellu obs\u0142ug\u0119 plik\xF3w CSV mo\u017Cna osi\u0105gn\u0105\u0107\
  \ za pomoc\u0105 biblioteki `cassava`, jednej z popularnych bibliotek stron trzecich\
  \ do tego celu."
title: Praca z plikami CSV
weight: 37
---

## Jak to zrobić:
W Haskellu obsługę plików CSV można osiągnąć za pomocą biblioteki `cassava`, jednej z popularnych bibliotek stron trzecich do tego celu. Poniżej przedstawiono przykłady pokazujące, jak czytać z plików CSV oraz jak do nich pisać, używając `cassava`.

**1. Odczytywanie pliku CSV:**

Najpierw upewnij się, że masz zainstalowaną `cassava`, dodając ją do pliku cabal Twojego projektu lub używając Stack.

Oto prosty przykład czytania pliku CSV i wyświetlania każdego rekordu. Zakładamy, że plik CSV ma dwie kolumny: nazwa i wiek.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

main :: IO ()
main = do
    csvData <- BL.readFile "people.csv"
    case decode NoHeader csvData of
        Left err -> putStrLn err
        Right v -> V.forM_ v $ \(name, age) ->
            putStrLn $ name ++ " ma " ++ show (age :: Int) ++ " lat."
```

Zakładając, że `people.csv` zawiera:
```
John,30
Jane,25
```
Wynik będzie:
```
John ma 30 lat.
Jane ma 25 lat.
```

**2. Tworzenie pliku CSV:**

Aby stworzyć plik CSV, możesz użyć funkcji `encode` z `cassava`.

Oto jak możesz zapisać listę rekordów do pliku CSV:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Csv
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = BL.writeFile "output.csv" $ encode [("John", 30), ("Jane", 25)]
```

Po uruchomieniu tego programu, `output.csv` będzie zawierać:

```
John,30
Jane,25
```

To zwięzłe wprowadzenie do pracy z plikami CSV w Haskellu, używając biblioteki `cassava`, demonstruje, jak czytać z plików CSV oraz jak do nich pisać, co czyni zadania manipulacji danymi bardziej przystępnymi dla osób nowych w języku.
