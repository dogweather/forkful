---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:30.196058-07:00
description: "Jak to zrobi\u0107: Od samego pocz\u0105tku, Haskell oferuje podstawowe\
  \ narz\u0119dzia do przetwarzania dat, ale wykorzystanie bibliotek takich jak `time`\
  \ dla\u2026"
lastmod: '2024-03-13T22:44:35.461718-06:00'
model: gpt-4-0125-preview
summary: "Od samego pocz\u0105tku, Haskell oferuje podstawowe narz\u0119dzia do przetwarzania\
  \ dat, ale wykorzystanie bibliotek takich jak `time` dla podstawowej funkcjonalno\u015B\
  ci oraz `date-parse` lub `time-parse` dla bardziej elastycznego przetwarzania mo\u017C\
  e znacz\u0105co upro\u015Bci\u0107 zadanie."
title: "Analiza sk\u0142adniowa daty z \u0142a\u0144cucha znak\xF3w"
weight: 30
---

## Jak to zrobić:
Od samego początku, Haskell oferuje podstawowe narzędzia do przetwarzania dat, ale wykorzystanie bibliotek takich jak `time` dla podstawowej funkcjonalności oraz `date-parse` lub `time-parse` dla bardziej elastycznego przetwarzania może znacząco uprościć zadanie.

Najpierw upewnij się, że masz dostępną bibliotekę `time`; jest ona często dołączana do GHC, ale jeśli musisz określić ją jako zależność, dodaj `time` do pliku cabal Twojego projektu lub użyj `cabal install time`, aby ręcznie ją zainstalować.

```haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

-- Używanie biblioteki time do przetworzenia daty w standardowym formacie
parseBasicDate :: String -> Maybe UTCTime
parseBasicDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" 
```

Przykład użycia i wynik:

```haskell
main :: IO ()
main = print $ parseBasicDate "2023-04-01"

-- Wynik: Just 2023-03-31 22:00:00 UTC
```

Dla bardziej złożonych scenariuszy, gdy musisz obsługiwać wiele formatów lub lokalizacji, biblioteki stron trzecich takie jak `date-parse` mogą być bardziej wygodne:

Zakładając, że dodałeś `date-parse` do swoich zależności i zainstalowałeś je, oto jak możesz ich użyć:

```haskell
import Data.Time.Calendar
import Text.Date.Parse (parseDate)

-- Przetwarzanie ciągu daty z biblioteką date-parse obsługuje wiele formatów
parseFlexibleDate :: String -> Maybe Day
parseFlexibleDate = parseDate
```

Przykład użycia z `date-parse`:

```haskell
main :: IO ()
main = print $ parseFlexibleDate "April 1, 2023"

-- Wynik: Just 2023-04-01
```

Każdy przykład demonstruje podstawowe podejście do przekształcania ciągu znaków w użyteczny obiekt daty w Haskellu. Wybór między korzystaniem z wbudowanych funkcji biblioteki `time` a opcją rozwiązania stron trzecich jak `date-parse` zależy od konkretnych potrzeb Twojej aplikacji, takich jak zakres formatów wejściowych, które musisz obsłużyć.
