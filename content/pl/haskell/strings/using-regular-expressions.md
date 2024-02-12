---
title:                "Korzystanie z wyrażeń regularnych"
aliases:
- /pl/haskell/using-regular-expressions.md
date:                  2024-02-03T19:17:04.476648-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z wyrażeń regularnych"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyrażenia regularne w programowaniu to sekwencje znaków definiujące wzorzec wyszukiwania, typowo wykorzystywane do przeszukiwania i manipulacji ciągami znaków. Programiści Haskell wykorzystują wyrażenia regularne do zadań, począwszy od prostego dopasowywania ciągów znaków po złożone przetwarzanie tekstu, wykorzystując ich efektywność i wszechstronność w obsłudze danych tekstowych.

## Jak to zrobić:
W Haskell funkcjonalności regex nie są częścią standardowej biblioteki, co wymaga używania pakietów stron trzecich takich jak `regex-base` wraz z kompatybilnym backendem jak `regex-posix` (dla wsparcia POSIX regex), `regex-pcre` (dla kompatybilnych z Perlem regex) itp. Oto jak możesz używać tych pakietów do pracy z wyrażeniami regularnymi.

Najpierw upewnij się, że masz zainstalowane pakiety, dodając `regex-posix` lub `regex-pcre` do pliku `.cabal` twojego projektu lub instalując bezpośrednio przez cabal:

```bash
cabal install regex-posix
```
lub
```bash
cabal install regex-pcre
```

### Użycie `regex-posix`:

```haskell
import Text.Regex.Posix ((=~))

-- Sprawdź, czy ciąg znaków pasuje do wzorca
isMatch :: String -> String -> Bool
isMatch text pattern = text =~ pattern :: Bool

-- Znajdź pierwsze dopasowanie
findFirst :: String -> String -> String
findFirst text pattern = text =~ pattern :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- Wynik: True
    print $ findFirst "dzień dobry, dobranoc" "dobry"
    -- Wynik: "dobry"
```

### Użycie `regex-pcre`:

```haskell
import Text.Regex.PCRE ((=~))

-- Znajdź wszystkie dopasowania
findAll :: String -> String -> [String]
findAll text pattern = text =~ pattern :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- Wynik: ["test1","test2","test3"]
```

Każda biblioteka ma swoje szczególności, ale ogólna metodologia użycia `=~` do stosowania regex pozostaje spójna, czy to przy sprawdzaniu dopasowania, czy przy ekstrakcji podciągów. Wybór między `regex-posix` a `regex-pcre` w dużej mierze zależy od potrzeb twojego projektu i specyficznych możliwości regex, których wymagasz.
