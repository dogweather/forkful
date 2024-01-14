---
title:    "Go: Ein Datum in der Zukunft oder Vergangenheit berechnen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Daten in der Zukunft oder Vergangenheit kann für Entwickler nützlich sein, wenn sie beispielsweise ein Programm schreiben, das auf bestimmte Ereignisse basiert oder Zeitintervalle berechnet. 

## Wie

Um ein Datum in der Zukunft oder Vergangenheit in Go zu berechnen, können wir die `time` Bibliothek nutzen. Hier ist ein Beispiel, das das Datum in 10 Tagen berechnet:

```Go
package main

import (
    "time"
    "fmt"
)

func main() {
    heute := time.Now() // Heutiges Datum
    zukunft := heute.AddDate(0, 0, 10) // Berechnetes Datum in 10 Tagen
    fmt.Println(zukunft.Format("2. Januar 2006")) // Ausgabe: 18. Juni 2021
}
```

Um ein Datum in der Vergangenheit zu berechnen, nutzen wir die `Sub()` Methode, die das Datum von einem angegebenen Zeitintervall subtrahiert. Hier ist ein Beispiel, das das Datum vor 2 Wochen berechnet:

```Go
package main

import (
    "time"
    "fmt"
)

func main() {
    heute := time.Now() // Heutiges Datum
    vergangenheit := heute.SubDate(0, 0, 14) // Berechnetes Datum vor 2 Wochen
    fmt.Println(vergangenheit.Format("2. Januar 2006")) // Ausgabe: 4. Juni 2021
}
```

## Deep Dive

Zusätzlich zu `AddDate()` und `SubDate()` gibt es weitere Methoden in der `time` Bibliothek, die es uns ermöglichen, Datumsberechnungen durchzuführen, wie zum Beispiel `Add()`, `Sub()` und `Date()`. Es ist auch möglich, Zeitangaben wie Stunden oder Minuten zu berechnen, indem wir die `Add()` oder `Sub()` Methode mit der `Duration` Funktion kombinieren.

## Siehe Auch

- [https://golang.org/pkg/time/](https://golang.org/pkg/time/)
- [https://www.callicoder.com/golang-datetime-format/](https://www.callicoder.com/golang-datetime-format/)
- [https://astaxie.gitbooks.io/build-web-application-with-golang/content/de/09.2.html](https://astaxie.gitbooks.io/build-web-application-with-golang/content/de/09.2.html)