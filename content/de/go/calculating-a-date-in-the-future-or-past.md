---
title:                "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
html_title:           "Go: Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Das Berechnen eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum jemand ein Datum in der Zukunft oder Vergangenheit berechnen möchte. Vielleicht müssen sie Termine planen, Fristen einhalten oder einfach nur neugierig darauf sein, an welchem Tag in der Zukunft ein bestimmtes Ereignis stattfinden wird.

## Wie geht's?
Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die Zeitpakete in Go verwenden:
```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    // Heute's Datum holen
    heute := time.Now()

    // Datum in der Vergangenheit berechnen
    vergangenheit := heute.AddDate(-1, 0, 0)

    // Datum in der Zukunft berechnen
    zukunft := heute.AddDate(0, 1, 0)

    // Formatieren und ausgeben
    fmt.Println("Vergangenes Datum:", vergangenheit.Format("02.01.2006"))
    fmt.Println("Zukünftiges Datum:", zukunft.Format("02.01.2006"))
}
```
Der Output wäre:
```
Vergangenes Datum: 19.02.2020
Zukünftiges Datum: 19.04.2020
```

## Tiefer eintauchen
Die `AddDate()` Funktion nimmt als Argumente das Jahr, den Monat und den Tag und gibt ein `time.Time` Objekt zurück. Wir können auch `Sub()` verwenden, um ein Datum in der Vergangenheit zu berechnen. Außerdem gibt es noch viele weitere Funktionen in den Zeitpaketen, um mit Datum und Uhrzeit in Go zu arbeiten.

## Siehe auch
- Dokumentation zu Zeitpaketen in Go: https://golang.org/pkg/time/
- Ein Tutorial zur Arbeit mit Datum und Uhrzeit in Go: https://yourbasic.org/golang/time-date-examples/#add-date