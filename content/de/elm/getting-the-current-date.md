---
title:                "Elm: Das heutige Datum erhalten"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

Das Abrufen des aktuellen Datums kann nützlich sein, um zeitbasierte Funktionen in Ihren Elm-Programmen zu integrieren. Zum Beispiel können Sie die aktuelle Uhrzeit auf einer Webseite anzeigen oder sie verwenden, um zu bestimmen, ob ein bestimmtes Ereignis in der Vergangenheit oder Zukunft liegt.

# Wie geht man vor

Um das aktuelle Datum in Elm zu erhalten, können Sie die `Date.now` Funktion verwenden. Diese Funktion gibt eine Zeitangabe im Millisekunden-Format zurück, die seit dem 1. Januar 1970, 00:00:00 UTC vergangen ist.

```Elm
import Date exposing (now)

-- Abrufen des aktuellen Datums und Speichern in Variable
currentDate = now

-- Ausgabe des aktuellen Datums im Format: YYYY-MM-DD
Html.text <| toString <| Date.toCalendarDate currentDate
```

Die Ausgabe sollte nun ähnlich aussehen: `2021-02-03`.

# Tiefergehende Informationen

Die `Date.now` Funktion gibt die Zeitangabe in Millisekunden zurück, jedoch in UTC (koordinierte Weltzeit). Wenn Sie das Datum in einer bestimmten Zeitzone anzeigen möchten, können Sie `Date.now` mit der Funktion `Date.fromZone` kombinieren. Diese Funktion erwartet zwei Argumente: die gewünschte Zeitzone und die Zeitangabe in Millisekunden. Hier ist ein Beispiel:

```Elm
import Date exposing (now, fromZone, Zone)

-- Abrufen des aktuellen Datums in der Zeitzone "Europe/Berlin"
currentDate = now
berlinTime = fromZone Zone.europeBerlin currentDate

-- Ausgabe des aktuellen Datums im Format: YYYY-MM-DD in Berliner Zeit
Html.text <| toString <| Date.toCalendarDate berlinTime
```

# Siehe auch

- [Date.now Dokumentation](https://package.elm-lang.org/packages/elm/time/latest/Date#now)
- [Date.fromZone Dokumentation](https://package.elm-lang.org/packages/elm/time/latest/Date#fromZone)
- [Uhrzeit anzeigen mit Elm](https://guide.elm-lang.org/effects/time.html#showing-current-time)