---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:14:24.358066-07:00
simple_title:         "Aktuelles Datum abrufen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Ermitteln des aktuellen Datums ist einfach das Feststellen, welches Datum heute ist. Programmierer brauchen es für alles Mögliche, vom Timestamping bis zur Terminplanung.

## Anleitung:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	heute := time.Now() // Gibt das aktuelle Datum und Uhrzeit zurück
	fmt.Println("Heutiges Datum:", heute.Format("02.01.2006"))
}
```

Beispielausgabe:
```
Heutiges Datum: 15.03.2023
```

## Vertiefung:
Das Ermitteln des aktuellen Datums ist so alt wie die Programmierung selbst. Die Verwendung der `time` Bibliothek in Go ist die bevorzugte Methode, es gibt jedoch Alternativen wie das Parsen von Datums-Strings oder den Einsatz von externen Paketen. Die Implementierung in Go nutzt unter der Haube Systemaufrufe, um die Uhrzeit des Betriebssystems zu erhalten. Verglichen mit anderen Sprachen, bietet Go eine robuste und konzise Möglichkeit, das Datum und die Uhrzeit zu handhaben.

## Siehe Auch:
- [`time` Paket Dokumentation](https://pkg.go.dev/time)
- [Go by Example: Time](https://gobyexample.com/time)
