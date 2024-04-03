---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:26.228536-07:00
description: "Wie geht das: In Go ist das `time`-Paket Ihr Zugangspunkt zur Arbeit\
  \ mit Daten und Zeiten. Die Funktion `time.Now()` gibt Ihnen das aktuelle Datum\
  \ und die\u2026"
lastmod: '2024-03-13T22:44:53.299406-06:00'
model: gpt-4-0125-preview
summary: In Go ist das `time`-Paket Ihr Zugangspunkt zur Arbeit mit Daten und Zeiten.
title: Das aktuelle Datum abrufen
weight: 29
---

## Wie geht das:
In Go ist das `time`-Paket Ihr Zugangspunkt zur Arbeit mit Daten und Zeiten. Die Funktion `time.Now()` gibt Ihnen das aktuelle Datum und die Uhrzeit, während andere Funktionen und Methoden es Ihnen ermöglichen, diese Daten zu formatieren oder zu manipulieren. So erhalten Sie das aktuelle Datum und seine verschiedenen Darstellungen:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	currentTime := time.Now() // Holt das aktuelle Datum und die Uhrzeit
	fmt.Println("Aktuelle Zeit:", currentTime)

	// Um das Datum im YYYY-MM-DD-Format zu erhalten
	fmt.Println("Aktuelles Datum:", currentTime.Format("2006-01-02"))

	// Um die einzelnen Komponenten des Datums zu erhalten
	year, month, day := currentTime.Date()
	fmt.Printf("Jahr: %d, Monat: %s, Tag: %d\n", year, month, day)

	// Um den Wochentag zu erhalten
	fmt.Println("Wochentag:", currentTime.Weekday())
}
```

Eine Beispiel-Ausgabe könnte so aussehen:

```
Aktuelle Zeit: 2023-04-18 15:04:05.123456 +0000 UTC
Aktuelles Datum: 2023-04-18
Jahr: 2023, Monat: April, Tag: 18
Wochentag: Dienstag
```

Beachten Sie, wie `Format` ein spezifisches Datum (2006-01-02) als Layoutzeichenfolge verwendet. Dies ist das von Go gewählte Referenzdatum, das als mnemonische Hilfe für die Formatierung von Daten dient.

## Vertiefung
Die Entscheidung, das `time`-Paket für die Datums- und Zeitmanipulation in Go zu verwenden, spiegelt die Hingabe der Sprache an robuste und intuitive Standardbibliotheken wider. Im Gegensatz zu einigen Sprachen, die möglicherweise mehrere konkurrierende Bibliotheken oder Methodologien für die Datumsmanipulation haben, legt Go Wert auf einen einzigen, gut dokumentierten Standard.

Die eigenwillige Wahl des Referenzdatums (`Mon Jan 2 15:04:05 MST 2006`) bei der Zeitformatierung von Go, obwohl anfänglich verwirrend, ist tatsächlich ein Geniestreich in der Benutzerfreundlichkeit. Es ermöglicht Programmierern, Datums- und Zeitformate mithilfe eines beispielbasierten Ansatzes darzustellen, anstatt Tokens oder Symbole zu memorieren, die andere Sprachen möglicherweise verwenden.

Allerdings, während das `time`-Paket umfassende Funktionalitäten für die meisten Bedürfnisse bietet, kann der Umgang mit Zeitzonen und DST (Daylight Saving Time) Änderungen manchmal neue Go-Programmierer stolpern lassen. Es ist entscheidend zu verstehen, wie Go ortspezifische Zeiten behandelt, um häufige Fallstricke bei der Zeitmanipulation zu vermeiden.

Für komplexere Terminplanungs- oder Zeitmanipulationsbedürfnisse könnten Drittanbieterbibliotheken wie `github.com/robfig/cron` für Go spezialisiertere Funktionalitäten als das Standard-`time`-Paket bieten. Für die meisten Anwendungen, die das Abrufen und den Umgang mit dem aktuellen Datum und der Uhrzeit erfordern, bietet das `time`-Paket jedoch einen soliden und idiomatischen Ausgangspunkt in Go.
