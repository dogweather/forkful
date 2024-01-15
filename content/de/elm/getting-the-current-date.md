---
title:                "Das aktuelle Datum erhalten"
html_title:           "Elm: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man das aktuelle Datum in Elm abrufen möchte. Möglicherweise benötigt man es für eine Kalenderfunktion oder zur Berechnung von Alters- oder Zeitunterschieden. Ganz gleich, aus welchem Grund, das Abrufen des aktuellen Datums ist ein häufiges Szenario in der Programmierung und kann mit Elm ganz einfach umgesetzt werden.

## Wie Geht's

Um das aktuelle Datum in Elm abzurufen, kann die vorinstallierte "Time" Bibliothek verwendet werden. Zunächst muss diese Bibliothek importiert werden, indem man folgende Zeile am Anfang des Codes einfügt:

```Elm
import Time
```

Anschließend kann man die folgende Funktion aufrufen, um das aktuelle Datum zu erhalten:

```Elm
Time.now
```

Um das Datum in einem bestimmten Format auszugeben, kann man die Funktion "format" verwenden. Diese Funktion erwartet zwei Parameter: das gewünschte Format als String und das Datum, welches formatiert werden soll.

Beispiel:

```Elm
Time.format "%d.%m.%Y" (Time.now)
```

Dieser Code gibt das aktuelle Datum im Format "TT.MM.JJJJ" zurück, zum Beispiel "29.11.2020". Es gibt auch andere Optionen für das Format, wie zum Beispiel "%Y-%m-%d" für das Format "JJJJ-MM-TT".

## Tiefer Einblick

Die "Time" Bibliothek basiert auf dem Unix-Zeitstempel, der die Anzahl der Sekunden seit dem 1. Januar 1970 darstellt. Dies ermöglicht eine einfache Umrechnung in andere Zeiteinheiten wie Stunden, Minuten oder Tage.

Es ist auch wichtig zu beachten, dass das Datum und die Uhrzeit abhängig von der Zeitzone des Computers sind, auf dem das Programm ausgeführt wird.

## Siehe Auch

- Offizielle Elm Dokumentation zur "Time" Bibliothek: https://package.elm-lang.org/packages/elm/time/latest/
- Ein Tutorial zur Verwendung von Datums- und Zeitfunktionen in Elm: https://www.elm-tutorial.org/en/09-subs-cmds/06-datetime.html