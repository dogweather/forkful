---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:36:21.324606-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"

category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Was & Warum?)
Das Parsen eines Datums aus einem String wandelt Text in ein `time.Time`-Objekt um. Programmierer nutzen dies, um mit Datumsangaben zu rechnen, sie zu vergleichen und in verschiedenen Formaten auszugeben.

## How to: (Wie geht's?)
Go bietet das Paket `time` für die Datum- und Zeitverarbeitung. Mit der `Parse`-Funktion kannst du einen String in ein `time.Time`-Objekt umwandeln.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	const layout = "02.01.2006 15:04"
	dateString := "25.12.2023 18:30"
	date, err := time.Parse(layout, dateString)
	if err != nil {
		panic(err)
	}
	fmt.Println("Parsed Date:", date)
}
```

Ausgabe:

```Go
Parsed Date: 2023-12-25 18:30:00 +0000 UTC
```

## Deep Dive (Tiefgang)
Golang verwendet ein einzigartiges Layoutsystem für Datum und Zeit. Anstatt komplizierte Formatierungszeichenfolgen zu verwenden, benutzt es eine spezifische Referenzzeit: `Mon Jan 2 15:04:05 MST 2006`. Alternative Methoden wie die Pakete `time.ParseInLocation` oder Drittanbieter wie `github.com/araddon/dateparse` können für komplexere Anforderungen genutzt werden.

Beim Parsen prüfen, ob das Layout exakt mit der Eingabe übereinstimmt. Beachte Zeitzonen -- `Parse` interpretiert das Datum in UTC, wenn keine Zeitzone angegeben ist.

## See Also (Siehe Auch)
- [time package](https://pkg.go.dev/time)
- [Go by Example: Time Formatting / Parsing](https://gobyexample.com/time-formatting-parsing)

Kein Schlussabschnitt nötig. Information ist vollständig.
