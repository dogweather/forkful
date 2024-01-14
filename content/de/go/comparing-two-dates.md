---
title:                "Go: Vergleich von zwei Datumsangaben"
simple_title:         "Vergleich von zwei Datumsangaben"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Möglicherweise fragen Sie sich, warum Sie überhaupt zwei Daten vergleichen müssen. Die Antwort ist einfach: Datumskomparationen sind ein häufiger Teil der Softwareentwicklung, insbesondere bei der Arbeit mit Zeit- und Datumswerten. Wenn Sie lernen, wie man zwei Daten vergleicht, können Sie komplexe Aufgaben wie die Suche nach dem frühesten oder spätesten Datum in einer Liste oder die Überprüfung, ob ein angegebenes Datum zwischen zwei anderen Daten liegt, ausführen.

## Wie geht das

Um zwei Daten in Go zu vergleichen, können Sie die `Before()`, `After()` oder `Equal()` Methoden des `time.Time` Pakets verwenden. Hier ist ein Beispiel, das die `After()` Methode verwendet, um zu überprüfen, ob ein Datum später als ein anderes Datum liegt:

```Go
date1 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)
date2 := time.Date(2020, time.December, 31, 23, 59, 59, 0, time.UTC)

if date1.After(date2) {
    fmt.Println("Date 1 is later than Date 2")
}
```

Dieser Code wird "Date 1 is later than Date 2" ausgeben, da der 1. Januar 2021 später als der 31. Dezember 2020 ist.

## Tiefer tauchen

Die `Before()`, `After()` und `Equal()` Methoden nutzen den UNIX-Zeitstempel, um die Daten zu vergleichen. Dies ist eine Zahl, die angibt, wie viele Sekunden seit dem 1. Januar 1970 vergangen sind. Wenn Sie also zwei Daten vergleichen, vergleichen Sie in Wirklichkeit zwei Zahlen. Dies bedeutet auch, dass Sie mit diesen Methoden nicht nur zwei `time.Time` Werte, sondern auch zwei Werte von anderen Datentypen vergleichen können, solange sie in einen UNIX-Zeitstempel umgewandelt werden können.

## Siehe auch

- [Go Time Package Dokumentation](https://golang.org/pkg/time/)
- [Offizielles Go Tutorial: Dates und Zeiten](https://tour.golang.org/time)
- [So bewältigt Go das Datumsdurcheinander](https://blog.gopheracademy.com/advent-2015/go-semantic-versioning-date-move/)