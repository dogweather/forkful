---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:55.074989-07:00
description: "Das Parsen eines Datums aus einem String in Elm beinhaltet das Umwandeln\
  \ von Textinformationen, die Daten und Zeiten repr\xE4sentieren, in ein Format,\
  \ das\u2026"
lastmod: '2024-02-25T18:49:50.876740-07:00'
model: gpt-4-0125-preview
summary: "Das Parsen eines Datums aus einem String in Elm beinhaltet das Umwandeln\
  \ von Textinformationen, die Daten und Zeiten repr\xE4sentieren, in ein Format,\
  \ das\u2026"
title: Einen Datum aus einem String analysieren
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String in Elm beinhaltet das Umwandeln von Textinformationen, die Daten und Zeiten repräsentieren, in ein Format, das Elm verstehen und manipulieren kann, speziell in den `Date` Typ. Dieser Prozess ist entscheidend für die Verarbeitung von Benutzereingaben, das korrekt lokalisierte Anzeigen von Daten und das Durchführen von datumsbezogenen Berechnungen. Er stellt sicher, dass Ihre Elm-Anwendungen zeitbezogene Daten intelligent verarbeiten können.

## Wie geht das:
Elm verfügt nicht über so robuste integrierte Fähigkeiten wie einige andere Sprachen zum Parsen von Daten, und verlässt sich hauptsächlich auf Javascript-Interop oder Bibliotheken für komplexere Operationen. Sie können jedoch das `elm/time` Paket für das grundlegende Parsen verwenden, und für komplexere Bedürfnisse wird die Drittanbieter-Bibliothek `justinmimbs/date` weit empfohlen.

### Parsen mit `elm/time`:
`elm/time` stellt das `Time` Modul zur Verfügung, welches es Ihnen erlaubt, mit Zeitstempeln anstelle von lesbaren Daten zu arbeiten. Obwohl es Daten nicht direkt aus Strings parst, können Sie einen ISO 8601 String in einen POSIX Zeitstempel umwandeln, mit dem Sie dann arbeiten können.

```elm
import Time exposing (Posix)

-- Angenommen, Sie haben einen ISO 8601 Datumsstring
isoDateStr : String
isoDateStr = "2023-01-01T00:00:00Z"

-- Konvertieren Sie ihn in einen POSIX Zeitstempel (diese Funktion gibt ein `Result` zurück)
parsedDate : Result String Posix
parsedDate = Time.fromIsoString8601 isoDateStr

-- Beispiel Ausgabe: Ok <posix Zeitwert>
```

### Parsen mit `justinmimbs/date`:
Für kompliziertere Parsvorgänge, wie zum Beispiel den Umgang mit Nicht-ISO-Formaten, ist die Bibliothek `justinmimbs/date` eine großartige Wahl. Hier ist, wie Sie sie verwenden können, um einen benutzerdefinierten Datumsstring zu parsen:

1. Stellen Sie sicher, dass Sie die Bibliothek installiert haben:

```shell
elm install justinmimbs/date
```

2. Verwenden Sie die Funktion `Date.fromString`, um benutzerdefinierte Datumsformate zu parsen:

```elm
import Date
import Result exposing (Result(..))

-- Angenommen, Sie haben ein benutzerdefiniertes Datumsformat `dd-MM-yyyy`
customDateStr : String
customDateStr = "01-01-2023"

-- Funktion zum Parsen des benutzerdefinierten Formats
parseDate : String -> Result String Date.Date
parseDate = Date.fromString "dd-MM-yyyy"

-- Beispielanwendung
parsedCustomDate : Result String Date.Date
parsedCustomDate = parseDate customDateStr

-- Beispiel Ausgabe: Ok (Date.fromCalendarDate 2023 Jan 1)
```

In diesen Beispielen kapselt der `Result` Typ entweder ein erfolgreiches Parsen, das ein Datum liefert (`Ok`), oder einen Fehler (`Err`), was eine robuste Fehlerbehandlung in Ihren Elm-Anwendungen ermöglicht.
