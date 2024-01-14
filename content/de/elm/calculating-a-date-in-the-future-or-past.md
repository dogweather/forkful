---
title:                "Elm: Ein Datum in der Zukunft oder Vergangenheit berechnen"
simple_title:         "Ein Datum in der Zukunft oder Vergangenheit berechnen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Datumsangaben in der Zukunft oder Vergangenheit kann eine praktische Möglichkeit sein, um bestimmte Aktionen zu automatisieren oder zu planen. Mit Elm können Sie dies auf einfache und elegante Weise tun.

## Wie geht das?

```Elm
import Time exposing ( add, hours, days, months, years, sub, inUtc )

-- Berechnet das Datum von heute in einer Woche
addWeek : Date
addWeek =
    Time.now
        |> inUtc
        |> add (days 7)

-- Berechnet das Datum von heute vor zwei Monaten
subTwoMonths : Date
subTwoMonths =
    Time.now
        |> inUtc
        |> sub (months 2)

-- Berechnet das Datum von heute in einem Jahr und fünf Monaten
addYearAndFiveMonths : Date
addYearAndFiveMonths =
    Time.now
        |> inUtc
        |> add (years 1)
        |> add (months 5)

-- Berechnet das Datum von heute vor einer Stunde
subHour : Date
subHour =
    Time.now
        |> inUtc
        |> sub (hours 1)
```

Die Funktionen `add` und `sub` ermöglichen es uns, Datumsangaben in der Zukunft oder Vergangenheit zu berechnen, indem wir einfach angeben, wie viele Einheiten (Stunden, Tage, Monate, Jahre) wir hinzufügen oder subtrahieren möchten. Wir können auch `inUtc` verwenden, um sicherzustellen, dass die Berechnungen in der UTC-Zeitzone durchgeführt werden.

## Tiefentauchen

Das Berechnen von Datumsangaben mag auf den ersten Blick einfach erscheinen, aber es gibt einige Dinge zu beachten. Zum Beispiel müssen wir berücksichtigen, dass einige Jahre Schaltjahre sind und daher einen zusätzlichen Tag haben. Für solche Fälle bietet Elm die Funktion `Time.isLeapYear`, die überprüft, ob ein bestimmtes Jahr ein Schaltjahr ist oder nicht.

Eine weitere wichtige Sache zu beachten ist, dass wir immer mit der UTC-Zeitzone arbeiten. Wenn wir also ein bestimmtes Datum in unserer lokalen Zeitzone anzeigen möchten, müssen wir eine Konvertierung vornehmen. Elm bietet die Funktion `Time.utcToOtherZone`, mit der wir ein Datum aus der UTC-Zeitzone in eine andere Zeitzone konvertieren können.

## Siehe auch

- [Offizielle Elm Dokumentation zu Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Blogbeitrag: Working with Dates in Elm](https://elmprogramming.com/working-with-dates-in-elm.html)
- [Video-Tutorial: Elm Time Library](https://www.youtube.com/watch?v=gJ_CIA4PqYo)