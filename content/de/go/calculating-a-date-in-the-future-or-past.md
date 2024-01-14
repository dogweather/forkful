---
title:                "Go: Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Warum

Das Berechnen von Daten in der Zukunft oder Vergangenheit ist eine nützliche Fähigkeit beim Programmieren. Es ermöglicht es uns, datumsbezogene Funktionen in unsere Programme einzubauen, um beispielsweise zu überprüfen, ob ein bestimmtes Datum in der Zukunft liegt oder um zukünftige Termine zu planen.

# Wie geht man vor

Um ein zukünftiges Datum zu berechnen, können wir die Funktion `AddDate()` aus der Paket `time` verwenden. Hier ist ein Beispiel, wie wir das Datum für morgen berechnen können:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Das heutige Datum erhalten
    heute := time.Now()

    // Morgen berechnen
    morgen := heute.AddDate(0, 0, 1)

    // Output: YYYY-MM-DD
    fmt.Println("Morgen ist", morgen.Format("2006-01-02"))
}
```

Das Ergebnis ist:

```
Morgen ist 2021-07-19
```

Um ein vergangenes Datum zu berechnen, können wir `Sub()` verwenden. Hier ist ein Beispiel, wie wir das Datum vor einer Woche berechnen können:

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Das heutige Datum erhalten
    heute := time.Now()

    // Vor einer Woche berechnen
    vorWoche := heute.Sub(7 * 24 * time.Hour)

    // Output: YYYY-MM-DD
    fmt.Println("Vor einer Woche war es", vorWoche.Format("2006-01-02"))
}
```

Das Ergebnis ist:

```
Vor einer Woche war es 2021-07-12
```

# Tiefere Einblicke

Es gibt noch viele weitere Möglichkeiten, Daten in der Zukunft oder Vergangenheit zu berechnen. Mit den Funktionen `Add()` und `Sub()` können wir auch andere Zeiteinheiten wie z.B. Stunden oder Monate berücksichtigen. Es ist auch möglich, Datumsangaben mit dem Format `2006-01-02` zu verändern.

# Siehe auch

- [Offizielle Dokumentation für das Paket "time"](https://golang.org/pkg/time/)
- [Go Tutorial: Datum und Zeit in Golang](https://tutorialedge.net/golang/go-date-time-tutorial/)
- [Praktische Beispiele für die Arbeit mit Datum und Zeit in Go](https://gobyexample.com/time)