---
title:                "Analiza składniowa daty z łańcucha znaków"
date:                  2024-02-03T19:14:26.516638-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa daty z łańcucha znaków"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Analiza daty z ciągu znaków w Elm polega na konwertowaniu informacji tekstowych przedstawiających daty i czasy na format, który Elm może zrozumieć i manipulować, a konkretnie na typ `Date`. Ten proces jest kluczowy do obsługi danych wejściowych użytkownika, poprawnego lokalizowanego wyświetlania dat oraz wykonywania obliczeń związanych z datami, zapewniając, że aplikacje Elm mogą inteligentnie przetwarzać dane czasowe.

## Jak to zrobić:
Elm nie posiada wbudowanych możliwości tak rozbudowanych jak niektóre inne języki do analizy dat, głównie polegając na interopie z Javascriptem lub bibliotekach do bardziej złożonych operacji. Jednak można użyć pakietu `elm/time` do podstawowej analizy, a dla bardziej skomplikowanych potrzeb, szeroko polecana jest biblioteka stron trzecich `justinmimbs/date`.

### Analiza przy użyciu `elm/time`:
`elm/time` udostępnia moduł `Time`, który pozwala pracować ze znacznikami czasu zamiast z datami czytelnymi dla człowieka. Chociaż nie analizuje bezpośrednio dat z ciągów znaków, można przekształcić ciąg ISO 8601 na znacznik czasu POSIX, z którym można następnie pracować.

```elm
import Time exposing (Posix)

-- Zakładając, że masz ciąg daty ISO 8601
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- Konwertuj go na znacznik czasu POSIX (ta funkcja zwraca `Result`)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- Przykładowe wyjście: Ok <wartość czasu posix>
```

### Analiza przy użyciu `justinmimbs/date`:
Dla bardziej złożonej analizy, jak radzenie sobie z formatami nie-ISO, biblioteka `justinmimbs/date` jest świetnym wyborem. Oto jak można jej użyć do analizy niestandardowego ciągu daty:

1. Upewnij się, że masz zainstalowaną bibliotekę:

```shell
elm install justinmimbs/date
```

2. Użyj funkcji `Date.fromString` do analizy niestandardowych formatów dat:

```elm
import Date
import Result exposing (Result(..))

-- Powiedzmy, że masz niestandardowy ciąg formatu daty `dd-MM-yyyy`
customDateStr : String
customDateStr = "01-01-2023"

-- Funkcja do analizy niestandardowego formatu
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- Przykładowe użycie
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- Przykładowe wyjście: Ok (Date.fromCalendarDate 2023 Jan 1)
```

W tych przykładach typ `Result` zawiera albo udaną analizę, która daje datę (`Ok`), albo błąd (`Err`), umożliwiając solidne obsługiwanie błędów w aplikacjach Elm.
