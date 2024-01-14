---
title:    "Elm: Das Abrufen des aktuellen Datums"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt damit beschäftigen, das aktuelle Datum in Elm zu bekommen? Nun, das aktuelle Datum ist ein wichtiger Teil vieler Anwendungen, sei es in einem Kalender oder als Teil einer Berechnung. Mit Elm kannst du ganz einfach das aktuelle Datum abrufen und in deiner Anwendung verwenden.

## How To

Um das aktuelle Datum in Elm zu bekommen, benötigst du das Modul `Time`. Dieses Modul bietet verschiedene Funktionen, um mit der Zeit in Elm zu arbeiten. Eine davon ist `now`, die das aktuelle Datum als Zeitstempel zurückgibt. Schauen wir uns ein Beispiel an:

```Elm
import Time

currentTime : Time.Posix.Time
currentTime =
    Time.now
```

In diesem Beispiel importieren wir das `Time` Modul und rufen dann `now` auf, um das aktuelle Datum zu bekommen. Es wird als ein Objekt vom Typ `Time.Posix.Time` zurückgegeben, das unter anderem den Tag, die Uhrzeit und den Wochentag enthält.

Um das Datum in einem bestimmten Format ausgeben zu können, können wir die Funktion `Date.fromTime` verwenden. Diese Funktion nimmt einen Zeitstempel als Argument und gibt eine `Date` Struktur zurück. Schauen wir uns ein Beispiel an:

```Elm
currentTime : Time.Posix.Time
currentTime =
    Time.now

currentDate : Date.Date
currentDate =
    Date.fromTime currentTime
```

Dieses Beispiel gibt das aktuelle Datum in einem standardmäßigen Format aus, zum Beispiel `2021-11-17`. Du kannst aber auch verschiedene Formatierungsfunktionen verwenden, um das Datum anders darzustellen.

```Elm
Date.toString Date.DayNumberMonthDay currentdate
-- gibt "November 17" aus
```

## Deep Dive

Es gibt noch viele weitere Funktionen und Möglichkeiten, um mit dem `Time` Modul in Elm zu arbeiten. Du kannst zum Beispiel auch bestimmte Zeiträume oder Zeitdifferenzen berechnen, indem du die Funktionen `add` und `subtract` verwendest. Außerdem gibt es auch das Modul `TimeZone`, mit dem du das Datum in verschiedenen Zeitzonen anzeigen lassen kannst.

Wenn du tiefer in die Materie eintauchen möchtest, empfehle ich dir, die offizielle Elm Dokumentation zum `Time` und `TimeZone` Modul zu lesen. Dort findest du weitere Beispiele und Erklärungen.

## Siehe auch

- [Offizielle Elm Dokumentation zum Time Modul](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Offizielle Elm Dokumentation zum TimeZone Modul](https://package.elm-lang.org/packages/elm/time/latest/TimeZone)