---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:04.476648-07:00
description: "Wyra\u017Cenia regularne w programowaniu to sekwencje znak\xF3w definiuj\u0105\
  ce wzorzec wyszukiwania, typowo wykorzystywane do przeszukiwania i manipulacji ci\u0105\
  gami\u2026"
lastmod: '2024-02-25T18:49:33.805600-07:00'
model: gpt-4-0125-preview
summary: "Wyra\u017Cenia regularne w programowaniu to sekwencje znak\xF3w definiuj\u0105\
  ce wzorzec wyszukiwania, typowo wykorzystywane do przeszukiwania i manipulacji ci\u0105\
  gami\u2026"
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
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
