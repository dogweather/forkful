---
title:                "Debug-Ausgaben drucken"
date:                  2024-01-20T17:52:30.703302-07:00
model:                 gpt-4-1106-preview
simple_title:         "Debug-Ausgaben drucken"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Debug-Ausgaben sind Informationen, die Ihr Programm zur Laufzeit ausgibt, um Ihnen zu helfen, Fehler zu verstehen und zu beheben. Programmierer nutzen sie, weil sie schnell aufzeigen, wo und vielleicht auch warum ein Programm nicht wie erwartet funktioniert.

## How to:
Go bietet das `fmt` Paket an, um Debug-Informationen einfach ausgeben zu können.

```Go
package main

import (
	"fmt"
)

func main() {
	variable := "Debug Info"
	// Einfache Debug-Ausgabe
	fmt.Println("Debug-Output:", variable)
	// Formatierter Debug-Output
	fmt.Printf("Debug-Output: %v\n", variable)
}
```
Das erzeugt folgende Ausgabe:
```
Debug-Output: Debug Info
Debug-Output: Debug Info
```

## Deep Dive
Seit jeher drucken Entwickler Debug-Informationen, um den Programmfluss zu verfolgen. Go bevorzugt einfache Ansätze wie `fmt.Println` und `fmt.Printf`. Alternativen wie das `log` Paket bieten mehr Kontrolle und Features, z.B. das Setzen von Log-Leveln oder das Ausgeben von Dateinamen und Zeilennummern. In Produktivsystemen könnten offizielle Logging-Frameworks wie `zap` oder `logrus` eingesetzt werden, die Performanz und strukturierte Logging-Möglichkeiten bieten.

Implementierungsdetail:

- `fmt.Println` setzt auf Variadik, d.h. es akzeptiert beliebig viele Argumente jedes Typs.
- `fmt.Printf` ermöglicht es, mit Formatierungs-Verben wie `%v` die Ausgabe zu kontrollieren.
- `log.Println` und `log.Printf` funktionieren ähnlich, hängen aber zusätzlich ein Datum und Zeitstempel an jede Ausgabe an.

Generell sollte Debug-Output vor dem Deployen in Produktion entfernt oder auskommentiert werden, um die Performance und Sicherheit zu gewährleisten.

## See Also
- Go’s offizielle `fmt` Dokumentation: https://pkg.go.dev/fmt
- Go’s `log` Paket Dokumentation: https://pkg.go.dev/log
- Logger-Bibliothek `zap`: https://github.com/uber-go/zap
- Logger-Bibliothek `logrus`: https://github.com/sirupsen/logrus

Lies Dich ein und wähle das passende Tool für Deine Anforderungen. Happy coding!
