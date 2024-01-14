---
title:    "Elm: Datum in einen String umwandeln"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Warum
Das Konvertieren von Datum und Uhrzeit in eine Zeichenfolge kann hilfreich sein, wenn man die Darstellung von Daten auf der Benutzeroberfläche anpassen möchte.

## Wie geht das?
```Elm
import Time exposing (format)
import Date exposing (Date, Gregorian)

dateToString : Date -> String
dateToString date =
    format "DD.MM.YYYY - HH:mm" (Gregorian.fromDate date)

```
Als Ergebnis erhalten wir beispielsweise die Zeichenfolge "01.01.2021 - 18:30" für den 1. Januar 2021 um 18:30 Uhr.

## Tiefergehende Analyse
Die `format` Funktion aus dem `Time` Modul und die `Date`-Datentype aus dem `Date`-Modul ermöglichen es uns, ein Datum in verschiedenen Formaten darzustellen. Dabei können wir die Formatierung der Zeichenfolge selbst definieren, indem wir Formatierungsstrings verwenden. Diese enthalten Platzhalter wie "DD" für den Tag, "MM" für den Monat oder "YYYY" für das Jahr. Durch die Kombination verschiedener Platzhalter und Trennzeichen können wir das Datum in der gewünschten Ausgabeform anzeigen lassen.

## Siehe auch
- Zeit- und Datums-Funktionen in Elm: https://package.elm-lang.org/packages/elm/time/latest/
- Dokumentation für das `Time`-Modul: https://package.elm-lang.org/packages/elm/time/latest/Time
- Dokumentation für das `Date`-Modul: https://package.elm-lang.org/packages/elm/date/latest/Date