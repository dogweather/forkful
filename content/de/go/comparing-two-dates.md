---
title:    "Go: Vergleich von zwei Daten"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum

In der Entwicklung von Software ist es oft notwendig, zwei Datumsangaben miteinander zu vergleichen. Sei es um zu prüfen, ob ein bestimmtes Datum bereits vergangen ist oder um zu ermitteln, wie viele Tage zwischen zwei Terminen liegen. In diesem Blogbeitrag zeigen wir, wie man in der Programmiersprache Go zwei Datumswerte vergleichen kann.

## Wie

Um zwei Datumsangaben in Go zu vergleichen, gibt es einige hilfreiche Funktionen in der Standardbibliothek. Eine davon ist `time.Date()`, mit der wir ein neues `time.Time`-Objekt erstellen können. Im folgenden Beispiel vergleichen wir zwei feste Datumsangaben und geben das Ergebnis aus:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {

	date1 := time.Date(2021, time.April, 15, 0, 0, 0, 0, time.UTC)
	date2 := time.Date(2021, time.May, 1, 0, 0, 0, 0, time.UTC)

	if date1.Before(date2) {
		fmt.Println("Das erste Datum liegt vor dem zweiten.")
	} else if date1.After(date2) {
		fmt.Println("Das erste Datum liegt nach dem zweiten.")
	} else {
		fmt.Println("Beide Daten sind gleich.")
	}
}
```

Die Ausgabe dieses Codes lautet: "Das erste Datum liegt vor dem zweiten." Das liegt daran, dass wir in unserem Beispiel `date2` auf den 1. Mai und `date1` auf den 15. April gesetzt haben.

Wir können auch die Funktion `time.Since()` nutzen, um die Differenz zwischen zwei Datumswerten zu berechnen. Im folgenden Beispiel berechnen wir die Tage zwischen heute und dem 1. Januar 2022:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {

	today := time.Now()
	newYearsDay := time.Date(2022, time.January, 1, 0, 0, 0, 0, time.UTC)

	days := int(time.Since(newYearsDay).Hours() / 24)

	fmt.Println("Es sind noch", days, "Tage bis zum 1. Januar 2022.")
}
```

Die Ausgabe lautet dann: "Es sind noch 180 Tage bis zum 1. Januar 2022."

## Deep Dive

Das Vergleichen von Datumswerten kann komplexer werden, wenn wir mit verschiedenen Zeitzonen arbeiten. Hier ist es wichtig, die korrekte Zeitzone zu verwenden, um unerwünschte Ergebnisse zu vermeiden.

Außerdem sollten wir immer die ursprünglichen Datumsangaben auf eine einheitliche Zeitzone - zum Beispiel UTC - konvertieren, um sie korrekt miteinander vergleichen zu können.

Eine häufige Fehlerquelle ist auch die Formatierung von Datumswerten. Mit `time.Parse()` können wir angeben, wie ein bestimmter Datumswert formatiert ist, um ihn dann in ein `time.Time`-Objekt umzuwandeln. Eine Liste aller verfügbaren Zeitformatierungen in Go findet man in der [Dokumentation](https://golang.org/pkg/time/#pkg-constants).

## Siehe auch

- [Offizielle Dokumentation zu Datums- und Uhrzeitberechnungen in Go](https://golang.org/pkg/time/)
- [Go Standard Libraries Cheat Sheet](https://medium.com/@cjus/cheat-sheet-go-standard-libraries-92211c1c0a14)
- [YouTube-Tutorial: Learn Go in 12 Minuten](https://www.youtube.com/watch?v=C8LgvuEBraI)