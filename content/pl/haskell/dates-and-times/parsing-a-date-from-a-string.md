---
title:                "Analiza składniowa daty z łańcucha znaków"
aliases:
- /pl/haskell/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:30.196058-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Przetwarzanie daty z ciągu znaków w Haskellu polega na przekształceniu tekstowych reprezentacji dat na strukturalny format, który program może manipulować. Ten proces jest fundamentalny dla aplikacji zajmujących się danymi kalendarzowymi, umożliwiając funkcje takie jak obliczanie czasu trwania, planowanie i walidację danych.

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
