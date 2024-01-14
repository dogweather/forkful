---
title:                "Go: Das aktuelle Datum abrufen."
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums und der Uhrzeit ist eine häufige Aufgabe bei der Entwicklung von Anwendungen. Es ist wichtig, da viele Anwendungen abhängig von der aktuellen Zeit Informationen anzeigen oder speichern müssen, wie z.B. die letzten Änderungen, Erstellung oder Aktualisierung von Daten.

## Wie man das aktuelle Datum in Go bekommt

Um das aktuelle Datum und die Uhrzeit in Go zu erhalten, müssen wir die `time` Bibliothek importieren und die `Now()` Funktion aufrufen. Diese Funktion gibt ein `time.Time` Objekt zurück, das das aktuelle Datum und die Uhrzeit repräsentiert.

````Go
package main

import (
	"fmt"
	"time"
)

func main() {
	current := time.Now()
	fmt.Println("Das aktuelle Datum und Uhrzeit ist:", current)
}
````
Die Ausgabe dieses Codes wäre ähnlich wie folgt: `Das aktuelle Datum und Uhrzeit ist: 2021-05-03 18:30:00 +0000 UTC`

Wir können auch nur das Datum oder die Uhrzeit separat abrufen, indem wir die entsprechenden Funktionen verwenden:

````Go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentDate := time.Now().Date()
	currentTime := time.Now().Time()
	fmt.Println("Heute ist der", currentDate, "und es ist", currentTime)
}
````

Die Ausgabe wäre in diesem Fall: `Heute ist der 2021-05-03 und es ist 18:30:00 +0000 UTC`

## Tiefergehende Informationen

Es ist wichtig zu wissen, dass das aktuelle Datum und die Uhrzeit von vielen Faktoren beeinflusst werden kann, wie z.B. der Zeitzone und dem Standort des Systems. Daher kann es hilfreich sein, sich mit der Dokumentation von `time.Now()` vertraut zu machen, um sicherzustellen, dass die Zeitangabe korrekt ist.

Eine weitere wichtige Methode in der `time` Bibliothek ist `Format()`, die es uns ermöglicht, das Datum und die Uhrzeit in einem bestimmten Format auszugeben. Zum Beispiel:

````Go
package main

import (
	"fmt"
	"time"
)

func main() {
	current := time.Now()
	formatted := current.Format("02.01.2006 15:04")
	fmt.Println("Das aktuelle Datum und die Uhrzeit in deutschem Format ist:", formatted)
}
````
Die Ausgabe wäre in diesem Fall: `Das aktuelle Datum und die Uhrzeit in deutschem Format ist: 03.05.2021 18:30`

## Siehe auch

- [Dokumentation von time.Now()](https://golang.org/pkg/time/#Now)
- [Go Zeitpakete von A bis Z](https://www.alexedwards.net/blog/an-overview-of-go-date-and-time-packages)