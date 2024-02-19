---
aliases:
- /de/elm/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:30:45.380514-07:00
description: "Das Berechnen von zuk\xFCnftigen oder vergangenen Daten bedeutet, einem\
  \ bestimmten Datum Tage, Monate oder Jahre hinzuzuf\xFCgen oder abzuziehen. Programmierer\u2026"
lastmod: 2024-02-18 23:09:04.795631
model: gpt-4-1106-preview
summary: "Das Berechnen von zuk\xFCnftigen oder vergangenen Daten bedeutet, einem\
  \ bestimmten Datum Tage, Monate oder Jahre hinzuzuf\xFCgen oder abzuziehen. Programmierer\u2026"
title: "Berechnung eines zuk\xFCnftigen oder vergangenen Datums"
---

{{< edit_this_page >}}

## Was & Warum?
Das Berechnen von zukünftigen oder vergangenen Daten bedeutet, einem bestimmten Datum Tage, Monate oder Jahre hinzuzufügen oder abzuziehen. Programmierer nutzen diese Funktion, um Fristen zu verwalten, Erinnerungen zu planen oder Zeitabstände zu analysieren.

## How to:
Elm bietet keine eingebaute Bibliothek für Datum-Manipulationen, wie JavaScripts `Date`, aber Pakete wie `justinmimbs/date` erweitern diese Funktionalität. Hier ein Beispiel, wie man mit diesem Paket ein Datum berechnen kann:

```Elm
import Date
import Date.Extra as DateExtra
import Time

-- Angenommen, heute ist der 1. Januar 2023
calculateDate : Date.Date
calculateDate =
    let
        today = Date.fromCalendarDate 2023 1 1
    in
    case today of
        Ok initialDate ->
            -- Addieren von 10 Tagen zum heutigen Datum
            DateExtra.add Days 10 initialDate

        Err _ ->
            -- Fehlerbehandlung, falls das Datum ungültig ist
            initialDate  -- Just use the initial date as a placeholder

-- Angenommen, calculateDate gibt `Ok 2023-01-11` zurück
```

Das Ergebnis der Funktion `calculateDate` wäre ein neues Datum, das 10 Tage in der Zukunft liegt – der 11. Januar 2023.

## Deep Dive:
Die direkte Manipulation von Daten ist in Elm nicht so einfach wie in Sprachen, die eine eingebaute Datum-Behandlung haben. Historisch betrachtet setzt Elm auf Immutable-Zustände, was bedeutet, dass Daten und Zustände nicht direkt verändert werden, sondern durch die Erstellung neuer Instanzen.

Elm-Pakete wie `justinmimbs/date` oder `rtfeldman/elm-iso8601-date-strings` bieten Funktionen an, um mit Daten zu arbeiten. Diese Pakete zielen darauf ab, die Arbeit mit Daten in Elm einfacher und sicherer zu machen, vermeiden aber die Mutable-Zustände von JavaScripts `Date` Objekt.

Die Implementierung von Datumsfunktionen in Elm bleibt explizit und kontrolliert durch die Nutzung von Types und Error Handling, was typisch für funktionale Programmiersprachen ist.

## See Also:
Hier sind einige nützliche Ressourcen für die Arbeit mit Daten in Elm:

- [justinmimbs/date documentation](http://package.elm-lang.org/packages/justinmimbs/date/latest)
- [rtfeldman/elm-iso8601-date-strings](http://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest)
- [Elm Lang - Time](https://package.elm-lang.org/packages/elm/time/latest/)
