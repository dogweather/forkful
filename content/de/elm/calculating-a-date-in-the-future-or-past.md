---
title:                "Berechnung eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Elm: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Berechnen eines zukünftigen oder vergangenen Datums ist ein übliches Bedürfnis in der Programmierung, um Zeitintervalle, wie das Alter oder Fälligkeitsdaten, zu verwalten.

# So geht's:

In Elm verwenden wir die Bibliothek `elm/time` und ihre `Time.Posix`-Module, um dies zu erreichen. Schauen wir uns an, wie wir ein Datum 7 Tage in der Zukunft berechnen können:

```Elm
import Time exposing (Posix, secondsToTime, timeToMillis, millisToTime)

addDays : Int -> Posix -> Posix
addDays days date =
    let
        millisPerDay = 86400000
        daysInMillis = toFloat (days * millisPerDay)
    in
    date
        |> timeToMillis
        |> ((+) daysInMillis)
        |> millisToTime
```

Rufen wir diese Funktion mit einem aktuellen Datum auf:

```Elm
import Time exposing (now)

calculateFutureDate : Cmd Msg
calculateFutureDate =
    Task.perform AdjustedDate (now |> Task.map (addDays 7))
```

Die Ausgabe ist ein neues Datum, das 7 Tage in der Zukunft liegt.

# Vertiefung

Das Managen von Daten ist seit den Anfängen der Programmierung ein zentrales Thema. Historisch gesehen gab es viele Ansätze, um dieses Problem zu lösen, von einfachen Unix-Zeitstempeln bis hin zu komplexen Datum-Bibliotheken.

Das Elm Time Modul ist ein solcher moderner Ansatz, es verwendet die POSIX-Zeit, ein Industriestandard, um Zeit und Datum zu behandeln. Das ermöglicht eine einfache und konsistente Handhabung quer durch verschiedene Plattformen und Sprachen.

Es gibt jedoch Alternativen wie die Libraries `elm-date-extra` und `justinmimbs/date`, die zusätzliche Funktionen oder unterschiedliche Ansätze zur Datumsberechnung bieten.

Neben dem `elm/time`-Paket, das wir hier verwendet haben, bietet Elm auch das `Date`-Paket an. Diese enthält eine Reihe von Funktionen zur Manipulation von Datums- und Zeitwerten und bietet möglicherweise eine einfachere Syntax für bestimmte Anwendungsfälle.

# Siehe auch

- [Elm Time Dokumentation](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elm Date Extra Dokumentation](https://package.elm-lang.org/packages/rluiten/elm-date-extra/latest/)
- [Justin Mimbs Date Dokumentation](https://package.elm-lang.org/packages/justinmimbs/date/latest/)