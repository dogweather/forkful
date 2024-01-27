---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Regularne wyrażenia to wzorce wykorzystywane do wyszukiwania i manipulowania tekstami. Programiści używają ich, aby szybko znajdować, weryfikować i przekształcać ciągi znaków, co jest kluczowe np. w przetwarzaniu danych czy walidacji inputu.

## Jak to zrobić:

Haskell używa pakietu `regex-base` w połączeniu z `regex-posix` (lub podobnych) do pracy z regularnymi wyrażeniami. Sprawdźmy to na przykładach:

```Haskell
import Text.Regex.Posix

-- Sprawdzanie czy string pasuje do wyrażenia
matchesPattern :: String -> String -> Bool
matchesPattern text pattern = text =~ pattern :: Bool

-- Wyszukiwanie wszystkich dopasowań
findAllMatches :: String -> String -> [String]
findAllMatches text pattern = text =~ pattern :: [String]

main :: IO ()
main = do
    let text1 = "programowanie w Haskell ma swój urok"
    let pattern1 = "ma"
    print $ matchesPattern text1 pattern1

    let text2 = "koty to fajne zwierzaki, tak jak koty syberyjskie"
    let pattern2 = "koty"
    print $ findAllMatches text2 pattern2
```

Wyjście:
```
True
["koty","koty"]
```

## Deep Dive

Regularne wyrażenia pojawiły się w latach 50., a w Haskellu dostępne są przez kilka bibliotek. Alternatywą dla nich są parsery, takie jak `Parsec` czy `Megaparsec`, które są potężniejsze, ale też trudniejsze w użytkowaniu. Implementacja w Haskellu wykorzystuje maszyny stanów i leniwe obliczenia, pozwalając na efektywne przetwarzanie danych tekstowych, nawet o dziwnych formatach.

## Zobacz też:

- Dokumentacja Hackage dla `regex-base`: http://hackage.haskell.org/package/regex-base
- Dokumentacja Hackage dla `regex-posix`: http://hackage.haskell.org/package/regex-posix
- Opis wyrażeń regularnych w Haskellu: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/regexes
- Wprowadzenie do parserów w Haskellu: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-values
