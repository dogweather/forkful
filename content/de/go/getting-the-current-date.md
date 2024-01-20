---
title:                "Das aktuelle Datum abrufen"
html_title:           "Gleam: Das aktuelle Datum abrufen"
simple_title:         "Das aktuelle Datum abrufen"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?

Der aktuelle Datumswert gibt einfach die gegenwärtige Datum und Uhrzeit zurück. Programmierer nutzen es oft für protokollierende Zwecke, um bestimmte Ereignisse zu verfolgen oder um Features wie Terminerinnerungen zu implementieren. 

## So geht's:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Aktuelles Datum bekommen
	jetzt := time.Now()
	fmt.Println("Aktuelles Datum: ", jetzt)
}
```
Ergebnis könnte wie folgt aussehen:

```Go
Aktuelles Datum:  2025-05-03 15:23:50.421796 +0200 CEST m=+0.000000001
```
##Deep Dive:

Historisch gesehen könnten wir in Go verschiedene Standardbibliotheken und Funktionen in der time Bibliothek verwenden, um das aktuelle Datum zu ermitteln. Alternativ könnten wir Drittanbieter-Pakete verwenden, die mehr Features und Flexibilität bieten könnten, aber für die meisten Anwendungen erfüllt die Standard time -Bibliothek unsere Bedürfnisse. Bei der Implementierung muss beachtet werden, dass Go's `time.Now()` automatisch die UTC-Zeitzone setzt; um die lokale Zeitzone zu verwenden, verwenden Sie `time.Now().Local()`. 

## Siehe auch:

- Go Official Documentation: [Time Package](https://golang.org/pkg/time/)
- StackOverflow: [How to format a time string in Go?](https://stackoverflow.com/questions/20234104/how-to-format-a-time-string-in-golang)\

Denken Sie daran, das offizielle Go-Dokument zu überprüfen, um die neuesten Best Practices und Updates zur Zeitbibliothek zu erhalten.