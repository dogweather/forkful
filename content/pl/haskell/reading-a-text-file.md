---
title:                "Haskell: Odczytywanie pliku tekstowego"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

Nia idziemy pisać o kodowaniu w języku Haskell. Zakres tego wpisu będzie skupiał się na czytaniu plików tekstowych i dlaczego jest to ważna umiejętność w programowaniu.

## Dlaczego

Czytanie plików tekstowych jest częstym zadaniem w programowaniu, ponieważ pozwala nam na łatwe i szybkie przetwarzanie oraz analizę dużej ilości danych. Ponadto, umiejętność czytania i zapisywania plików jest niezbędna w wielu projektach.

## Jak to zrobić

Przykładowym sposobem na odczytywanie plików tekstowych w Haskell jest użycie funkcji `readFile`, która przyjmuje jako argument ścieżkę do pliku i zwraca jego zawartość w postaci tekstu. Poniżej przedstawiamy prosty przykład użycia tej funkcji:

```Haskell
fileContent <- readFile "plik.txt"
```

Aby następnie móc przetwarzać i manipulować odczytanymi danymi, konieczne jest przekształcenie ich z postaci tekstu na odpowiedni typ danych. W tym celu możemy wykorzystać funkcję `lines`, która podzieli tekst na pojedyncze wiersze, lub funkcję `words`, która podzieli tekst na pojedyncze słowa. Poniżej znajdują się przykładowe definicje tych funkcji oraz sposób ich zastosowania:

```Haskell
lines :: String -> [String]
words :: String -> [String]

-- Przykład użycia:
let lines = lines "Ten tekst zostanie podzielony na\nwiersze"
let words = words "Ten tekst zostanie podzielony na pojedyncze słowa"
```

## Głębsza analiza

Podczas czytania plików tekstowych, często zdarza się, że musimy przetwarzać jedynie wybrane linie lub słowa. Wtedy przydatna może okazać się funkcja `filter`, która pozwala na wybranie konkretnych elementów z listy lub tekst. Oto kilka przykładowych zastosowań tej funkcji:

```Haskell
filter :: (a -> Bool) -> [a] -> [a]

-- Przykład 1: Wybranie tylko liczb całkowitych:
let oddNumbers = filter (\x -> x `mod` 2 == 1) [1,2,3,4,5] -- [1,3,5]

-- Przykład 2: Wybranie tylko linii zaczynających się od określonego słowa:
let linesWithPrefix = filter (\x -> head x == "Haskell") ["Haskell jest fajny", "Haskell to język programowania", "Java to też fajny język"] -- ["Haskell jest fajny", "Haskell to język programowania"]
```

## Zobacz też

- [Dokumentacja funkcji `readFile` w języku Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html#v:readFile)
- [Przykłady użycia funkcji `readFile` i `lines`](https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter-9/files/ex.hs)
- [Więcej informacji o funkcjach `filter`, `lines` i `words`](https://www.haskell.org/onlinereport/standard-prelude.html)