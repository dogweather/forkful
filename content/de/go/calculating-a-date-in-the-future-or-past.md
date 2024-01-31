---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
date:                  2024-01-20T17:31:02.059595-07:00
model:                 gpt-4-1106-preview
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"

category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Berechnen eines zukünftigen oder vergangenen Datums ermöglicht es uns, Zeitintervalle zu handhaben, etwa für Terminplanungen, Erinnerungsfunktionen oder Gültigkeitsdauern. Programmierer verwenden solche Berechnungen, um Zeitspannen zu verwalten, Automatisierungen zu steuern und Benutzerinteraktionen zeitlich zu koordinieren.

## How to:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	heute := time.Now()
	fmt.Println("Heute:", heute.Format("02.01.2006"))

	zukunft := heute.Add(48 * time.Hour) // +2 Tage
	fmt.Println("Zukunft:", zukunft.Format("02.01.2006"))

	vergangenheit := heute.Add(-24 * time.Hour) // -1 Tag
	fmt.Println("Vergangenheit:", vergangenheit.Format("02.01.2006"))
}
```

Ausgabe:
```
Heute: 24.03.2023
Zukunft: 26.03.2023
Vergangenheit: 23.03.2023
```

## Deep Dive

Die Zeithandhabung in Go erfolgt primär durch das `time` Paket. Das hat historisch bedingt viele Vorzüge und ist gerade für Zeitzonen und Sommer-/Winterzeitumstellungen robust entwickelt worden. Alternativen sind unter anderem bibliotheksexterne Pakete, wie `dateparse` für mehr Flexibilität beim Parsen von Datumsangaben oder `cron` für zeitbasierte Jobs.

In der Implementierung ist das Rechnen mit Zeit und Datum dank Go's `Add` Funktion des `time.Time` Typs unkompliziert. Für das Erweitern oder Reduzieren von Datumsangaben werden oft `Duration` Typen verwendet, die mit Zeiteinheiten wie Stunden (`time.Hour`), Minuten (`time.Minute`) und Sekunden (`time.Second`) arbeiten.

## See Also

- Go by Example: Time (https://gobyexample.com/time)
- Go `time` package documentation (https://golang.org/pkg/time/)
- Go Time Formatting and Parsing (https://golang.org/src/time/format.go)
