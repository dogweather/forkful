---
title:                "Gleam: Vergleich zweier Daten"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Warum

Das Vergleichen von zwei Daten ist ein wichtiger Teil der Programmierung, der dabei hilft, zeitliche Abhängigkeiten zu erkennen und zu verarbeiten. Es ermöglicht auch die Durchführung von Aktionen basierend auf dem Vergleich von zwei Datumsangaben, wie zum Beispiel das Festlegen von Ablaufdaten oder das Sortieren von Daten in chronologischer Reihenfolge.

# Wie man es macht

Um zwei Daten in Gleam zu vergleichen, können Sie die `date.compare()` Funktion verwenden. Diese Funktion nimmt zwei Datumswerte als Argumente und gibt entweder 0, 1 oder -1 zurück, je nachdem, ob das erste Datum gleich, größer oder kleiner als das zweite Datum ist. Hier ist ein Beispiel, das zeigt, wie Sie diese Funktion verwenden können:

```Gleam
let date1 = date.new(2021, 1, 1)
let date2 = date.new(2021, 2, 1)

let result = date.compare(date1, date2)
```

In diesem Beispiel lautet die Ausgabe `result = -1`, da `date1` kleiner als `date2` ist.

# Tiefer Einblick

Beim Vergleichen von Daten gibt es einige wichtige Dinge zu beachten. Zum einen müssen die Daten im gleichen Format vorliegen, damit sie sinnvoll verglichen werden können. Gleam bietet verschiedene Funktionen, um Datumswerte in das gewünschte Format umzuwandeln, wie zum Beispiel die `date.from_string()` Funktion.

Außerdem ist es wichtig, die Zeitzone zu berücksichtigen, wenn Sie Daten vergleichen. Wenn Sie zum Beispiel Daten von verschiedenen Orten vergleichen, kann dies zu unerwarteten Ergebnissen führen, da die Zeitzone unterschiedlich sein kann.

In Gleam gibt es auch die Möglichkeit, benutzerdefinierte Vergleichsfunktionen zu erstellen, um spezifische Vergleiche durchzuführen, beispielsweise basierend auf spezifischen Kriterien für Zeit oder Datum.

# Siehe auch

- [Gleam Dokumentation zu Datumswerten](https://gleam.run/book/tour/dates)
- [Formatierung von Daten in Gleam](https://gleam.run/book/core/date.html#format)
- [Zeitzonenumrechnung in Gleam](https://gleam.run/book/library/chronos)