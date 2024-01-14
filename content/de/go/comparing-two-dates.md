---
title:                "Go: Vergleich von zwei Datumsangaben"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist ein grundlegender Teil der Programmierung und kann in vielen Anwendungsfällen nützlich sein. Zum Beispiel kann es verwendet werden, um zu überprüfen, ob ein bestimmtes Datum in der Vergangenheit liegt oder um die Aktualität von Daten zu überprüfen. Erfahren Sie in diesem Artikel, wie Sie mithilfe von Go Programmierung zwei Daten vergleichen können.

## Wie man vergleicht

Um zwei Daten in Go zu vergleichen, können Sie die Funktion `Before`, `After` oder `Equal` aus der Standardbibliothek `time` verwenden. Diese Funktionen nehmen zwei Daten als Parameter und geben eine boolsche Aussage zurück, die angibt, ob das erste Datum vor, nach oder gleich dem zweiten Datum liegt. Hier ist ein Beispielcode:

```
package main

import (
    "fmt"
    "time"
)

func main() {
    date1 := time.Date(2020, time.October, 1, 0, 0, 0, 0, time.UTC)
    date2 := time.Date(2021, time.March, 15, 0, 0, 0, 0, time.UTC)

    // Date1 liegt vor Date2
    fmt.Println(date1.Before(date2)) // Ausgabe: true

    // Date1 liegt nach Date2
    fmt.Println(date1.After(date2)) // Ausgabe: false

    // Date1 ist gleich Date2
    fmt.Println(date1.Equal(date2)) // Ausgabe: false
}
```

In diesem Beispiel haben wir zwei Daten erstellt und sie dann mit den drei Funktionen verglichen. Die Ausgabe zeigt das erwartete Ergebnis bei jedem Vergleich.

## Tiefergehende Informationen

Beim Vergleichen von zwei Daten in Go gibt es einige Dinge zu beachten. Zum Beispiel ist es wichtig, sicherzustellen, dass beide Daten im gleichen Zeitzonen-Format sind, da dies das Ergebnis des Vergleichs beeinflussen kann. Die Funktionen `Before`, `After` und `Equal` vergleichen auch die genaue Zeit und nicht nur das Datum. Wenn Sie also nur das Datum vergleichen möchten, müssen Sie die Zeiten vor dem Vergleich auf Null setzen.

Wenn Sie über `time.Duration` Vergleiche durchführen möchten, können Sie die Funktion `Sub` verwenden, um die Differenz zwischen zwei Daten zu erhalten. Diese Differenz kann dann mit den gewünschten Zeitintervallen verglichen werden.

## Siehe auch

- Offizielle Dokumentation zu `time` Paket: https://golang.org/pkg/time/
- Tutorial zum Vergleichen von Daten in Go: https://www.digitalocean.com/community/tutorials/how-to-compare-dates-in-go
- Weitere nützliche Funktionen aus dem `time` Paket: https://golang.org/pkg/time/#pkg-overview