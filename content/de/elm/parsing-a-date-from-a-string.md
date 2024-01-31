---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:35:41.263372-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"

category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Datum-Parsing bedeutet, ein Textdatum in ein strukturiertes Format zu verwandeln – nützlich, weil Daten oft als Strings übertragen, aber als Datumstypen verarbeitet werden.

## How to:
Im Elm, ein einfaches Beispiel mit dem Paket `justinmimbs/date`:

```Elm
import Date
import Date.Extra.Parse exposing (isoString)

-- Beispielfunktion zum Parsen eines ISO 8601 Datum-Strings
parseDate : String -> Maybe Date.Date
parseDate dateString =
    isoString dateString

-- Verwende diese Funktion
parsedDate : Maybe Date.Date
parsedDate =
    parseDate "2021-11-01T12:45:00Z"
    
-- Resultat ist Maybe Date.Date, also Nothing oder Just Date.Date
```

## Deep Dive
Historisch gesehen hatte Elm im Kern nicht den besten Support für Datum-Parsing, daher der Rückgriff auf Community-Pakete wie `justinmimbs/date`. Alternative Methoden beinhalten das Schreiben eigener Parser oder das Verwenden von JavaScript interop. Die Verarbeitung der Zeitzone ist oft ein Stolperstein beim Datum-Parsing, deshalb achte auf ISO 8601 Strings, die dieses Problem standardisieren.

## See Also
- Elm Date Paket: [package.elm-lang.org/packages/justinmimbs/date/latest/](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
- Elm ISO 8601 Date Strings: [package.elm-lang.org/packages/justinmimbs/date/latest/Date-Extra-Parse#isoString](https://package.elm-lang.org/packages/justinmimbs/date/latest/Date-Extra-Parse#isoString)
- Elm's Time Paket: [package.elm-lang.org/packages/elm/time/latest/](https://package.elm-lang.org/packages/elm/time/latest/)
