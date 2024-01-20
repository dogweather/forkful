---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
html_title:           "Go: Berechnung eines zukünftigen oder vergangenen Datums"
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Bestimmen eines Datums in der Zukunft oder Vergangenheit ist einfach die Berechnung eines Datums basierend auf einem bestimmten Intervall von einem Ausgangsdatum. Programmierer tun dies oft, um Ereignisse zu planen oder um Vergleiche zwischen Daten zu ermöglichen.

## So geht's:

In Go können wir die Funktion `Add` und `Sub` verwenden, um Daten zu berechnen. Hier ist ein Beispiel:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	now := time.Now()

	// Berechne Datum in der Zukunft
	future := now.AddDate(1, 0, 0)
	fmt.Println("In einem Jahr:", future)

	// Berechne Datum in der Vergangenheit
	past := now.AddDate(-1, 0, 0)
	fmt.Println("Vor einem Jahr:", past)
}
```

Wenn Sie das Programm ausführen, erhalten Sie eine Ausgabe ähnlich der folgenden:

```
In einem Jahr: 2023-01-05 12:04:05.678901234 +0200 EET
Vor einem Jahr: 2021-01-05 12:04:05.678901234 +0200 EET


## Tief Tauchen:

Historisch gesehen war die Berechnung eines Datums in der Vergangenheit oder Zukunft eine komplexe Aufgabe, die oft manuell durchgeführt wurde. In modernen Sprachen wie Go ist dies jedoch erheblich vereinfacht.

Es gibt auch alternative Methoden, um dies zu erreichen. Zum Beispiel können Sie Zeitdauern erstellen und sie mit der aktuellen Zeit addieren oder subtrahieren.

Die `AddDate`-Methode arbeitet, indem sie das Jahr, den Monat und den Tag getrennt hinzufügt. Das bedeutet, dass sie zuerst ein Jahr hinzufügt, dann einen Monat, und schließlich einen Tag. Es berücksichtigt auch Schaltjahre und die verschiedenen Anzahl von Tagen in einem Monat.

## Siehe Auch:

Hier sind einige hilfreiche Ressourcen, um mehr zu erfahren:

- Go-Dokumentation für das time-Paket: https://golang.org/pkg/time/
- Ausführlicher Artikel über die Handhabung von Zeit und Datum in Go: https://www.ardanlabs.com/blog/2019/03/managing-date-time-and-timezones-in-go.html
- Stack Overflow-Thread über das Hinzufügen von Tagen zu einem Datum: https://stackoverflow.com/questions/37696725/add-days-to-current-time-in-go