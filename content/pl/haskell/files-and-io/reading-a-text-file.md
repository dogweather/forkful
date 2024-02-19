---
aliases:
- /pl/haskell/reading-a-text-file/
date: 2024-01-20 17:54:28.202213-07:00
description: "Czytanie pliku tekstowego to proces pobierania danych z pliku przechowywanego\
  \ na dysku. Programi\u015Bci robi\u0105 to, by za\u0142adowa\u0107 i przetworzy\u0107\
  \ informacje,\u2026"
lastmod: 2024-02-18 23:08:49.670162
model: gpt-4-1106-preview
summary: "Czytanie pliku tekstowego to proces pobierania danych z pliku przechowywanego\
  \ na dysku. Programi\u015Bci robi\u0105 to, by za\u0142adowa\u0107 i przetworzy\u0107\
  \ informacje,\u2026"
title: Odczytywanie pliku tekstowego
---

{{< edit_this_page >}}

## What & Why?
Czytanie pliku tekstowego to proces pobierania danych z pliku przechowywanego na dysku. Programiści robią to, by załadować i przetworzyć informacje, często konieczne do działania ich aplikacji.

## How to:
Czytanie pliku tekstowego w Haskellu jest proste. Użyjemy funkcji `readFile`, którą dostarcza standardowa biblioteka.

```Haskell
main :: IO ()
main = do
    content <- readFile "example.txt"
    putStrLn content
```

Jeśli plik "example.txt" zawiera tekst "Cześć, Haskell!", wyjście będzie wyglądać tak:
```
Cześć, Haskell!
```

A co jeśli chcesz pracować z każdą linijką osobno? Użyj `lines`:

```Haskell
main :: IO ()
main = do
    content <- readFile "example.txt"
    let linesOfContent = lines content
    mapM_ putStrLn linesOfContent
```

Jeżeli "example.txt" ma wiele linii, każda z nich zostanie wypisana osobno.

## Deep Dive
Funkcja `readFile` pojawiła się w Haskellu 98 i od tamtej pory jest standardowym sposobem na odczytywanie plików tekstowych. Jest prosta, ale ma swoje ograniczenia, na przykład nie radzi sobie z dużymi plikami – lepiej wtedy użyć `hGetContents` i obsługi strumieni.

`readFile` korzysta z leniwej ewaluacji, co oznacza że zawartość pliku jest wczytywana tylko wtedy, gdy jest potrzebna. Dzięki temu małe pliki są szybko przetwarzane, ale przy dużych plikach to może prowadzić do problemów z wydajnością.

Alternatywą dla `readFile` jest `readFile'` z biblioteki `strict`, która wczytuje cały plik naraz – dobre rozwiązanie dla dużych plików.

A kwestia bezpieczeństwa? Funkcje takie jak `withFile` automatycznie zamykają plik po zakończeniu pracy, której zakres określamy. Przykład:

```Haskell
import System.IO

main :: IO ()
main = withFile "example.txt" ReadMode (\handle -> do
    content <- hGetContents handle
    putStr content)
```

Bezpieczniej, bo nie musimy pamiętać o zamykaniu pliku.

## See Also
- Haskell 2010 Language Report: https://www.haskell.org/onlinereport/haskell2010/
- Hackage, biblioteka pakietów Haskell: https://hackage.haskell.org/
- Learn You a Haskell for Great Good!, przystępny tutorial dla Haskell: http://learnyouahaskell.com/chapters
- Real World Haskell, książka do praktycznej nauki Haskell: http://book.realworldhaskell.org/
- "Haskell Programming from first principles" – kompleksowe wprowadzenie do języka: http://haskellbook.com/
