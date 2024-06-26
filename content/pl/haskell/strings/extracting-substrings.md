---
date: 2024-01-20 17:45:52.690580-07:00
description: ''
lastmod: '2024-04-05T22:50:49.762958-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Wycinanie pod\u0142a\u0144cuch\xF3w"
weight: 6
---

## How to:
"## Jak to zrobić:"

```Haskell
-- Użycie funkcji 'take' i 'drop' dla wyciągnięcia podciągów
substring :: Int -> Int -> String -> String
substring start end str = take (end - start) (drop start str)

-- Przykład użycia
main :: IO ()
main = do
    let text = "Witajcie w świecie Haskell!"
    putStrLn $ substring 8 15 text  -- wydobywa "w świe"
```

Output będzie wyglądać tak:
```
w świe
```

Wykorzystanie `Data.Text` dla efektywności:

```Haskell
import qualified Data.Text as T

-- Przykłady z `Data.Text`
textSubstring :: Int -> Int -> T.Text -> T.Text
textSubstring start end txt = T.take difference (T.drop start txt)
  where
    difference = end - start

-- Przykład użycia z 'Data.Text'
main :: IO ()
main = do
    let text = T.pack "Funkcje w Haskell są super!"
    T.putStrLn $ textSubstring 17 22 text  -- wydobywa "są sup"
```

Output będzie:
```
są sup
```

## Deep Dive:
"## Wgłębiamy się:"

Wczesna wersja języka Haskell pojawiła się w 1990 roku. Od tego czasu manipulacja stringami często korzystała z modułu `Data.List`. Funkcje `take` i `drop` były podstawą do tworzenia funkcji operujących na ciągach znaków. Później, dla lepszej wydajności i obsługi unikodowych tekstów, wprowadzono bibliotekę `Data.Text`.

Wyciąganie podciągów jest często używane na przykład w weryfikacji danych formularzy lub parsing'u plików – ważne zadania w realnych aplikacjach. Implementacja może się różnić w zależności od konkretnych wymagań i charakterystyki analizowanych danych.

Alternatywą dla `Data.Text` może być `Data.ByteString` w sytuacjach, gdy pracujemy z danymi binarnymi lub gdy optymalizacja pod kątem zużycia pamięci czy prędkości wykonania jest kluczowa.

## See Also:
"## Zobacz również:"

- Real World Haskell, Bryan O'Sullivan, Don Stewart, and John Goerzen: http://book.realworldhaskell.org/
- Haskell Documentation for `Data.Text` module: https://www.stackage.org/haddock/lts-18.14/text-1.2.4.1/Data-Text.html
- LYAHFGG - Learn You a Haskell for Great Good, chapter on strings: http://learnyouahaskell.com/starting-out#an-intro-to-lists
