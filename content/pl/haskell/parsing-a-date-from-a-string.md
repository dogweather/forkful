---
title:                "Analiza daty z ciągu znaków"
html_title:           "Haskell: Analiza daty z ciągu znaków"
simple_title:         "Analiza daty z ciągu znaków"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Paroszerowanie daty z ciągu znaków to proces, w którym programista przekształca dane zapisane w postaci tekstu na obiekty daty, które mogą być przetwarzane przez program. Jest to przydatne w wielu przypadkach, np. w systemach rezerwacji, przetwarzaniu danych finansowych lub w prostych aplikacjach kalendarza.

## Jak to zrobić:

```Haskell
import Data.Time.Format

-- funkcja do parsowania daty z ciągu znaków w formacie ISO
parseISODate :: String -> Maybe UTCTime
parseISODate = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"

-- przykładowe użycie
-- przykładowy ciąg znaków: "2021-06-15T16:30:00+02:00"
parseISODate "2021-06-15T16:30:00+02:00"
-- wynik: Just 2021-06-15 16:30:00 CEST
```

## Głębszy zanurzenie:

Paroszerowanie daty z ciągu znaków jest szerszym zagadnieniem związanym z przetwarzaniem danych. Historia parsowania sięga lat 70., gdy popularne stało się przetwarzanie danych w formie tekstowej. Alternatywnym sposobem na obsługę dat jest wykorzystanie bibliotek do obsługi czasu i stref czasowych, takich jak ```Data.Time``` w języku Haskell. W implementacji parsowania daty z ciągu znaków ważną rolę odgrywa formatowanie i analiza tekstu w celu wydobycia informacji o dacie.

## Zobacz także:

- Dokumentacja biblioteki Data.Time.Format: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time-Format.html
- Wprowadzenie do parsowania daty w języku Haskell: https://wiki.haskell.org/Parsing_a_datetime
- Porównanie różnych sposobów obsługi dat w języku Haskell: https://www.yesodweb.com/book/time-and-date