---
title:                "Datum in einen String umwandeln"
date:                  2024-01-20T17:36:41.002793-07:00
model:                 gpt-4-1106-preview
simple_title:         "Datum in einen String umwandeln"

category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Wozu?
Das Umwandeln eines Datums in einen String ermöglicht es, Datumsinformationen in einem menschenlesbaren Format zu speichern oder anzuzeigen. Programmierer machen das häufig, um Datumsangaben in Logdateien, Benutzerschnittstellen oder beim Datenexport zu verwenden.

## Anleitung:
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Aktuelles Datum und Zeit erhalten
	now := time.Now()

	// Datum und Zeit als String formatieren
	dateStr := now.Format("2006-01-02 15:04:05")
	fmt.Println("Datum und Zeit als String:", dateStr)

	// Auch möglich: Formatierung angepasst für DE
	deDateStr := now.Format("02.01.2006 15:04")
	fmt.Println("Datum im DE-Format:", deDateStr)
}
```
Ausgabe:
```
Datum und Zeit als String: 2023-04-12 08:41:20
Datum im DE-Format: 12.04.2023 08:41
```

## Vertiefung:
Die Formatierung von Datum und Zeit in Go wurde ausgehend von dem Paket `time` entwickelt. Das Referenzdatum in Go – also das, nach dem sich die Formatierungsanweisungen richten – ist `Mon Jan 2 15:04:05 MST 2006`, was einem Zeitstempel mit allen notwendigen Platzhaltern entspricht. Für manche mag dies eigentümlich erscheinen, aber einmal verstanden, ist es ein mächtiges Werkzeug.

Es gibt Alternativen zur `time`-Paketformatierung, wie zum Beispiel Stringfunktionen oder Pakete von Drittanbietern, aber die `time`-Paketmethode ist standardisiert, gut unterstützt und wird empfohlen.

Beim Export von Daten in unterschiedliche Länder oder Systeme ist es oft nötig, das Datumsformat anzupassen. Daher ist es wichtig, dass Go Anpassungen zulässt, wie zum Beispiel das deutsche Datumsformat `Tag.Monat.Jahr`.

## Weiterführendes:
- Go Dokumentation zum `time` Paket: [time package](https://pkg.go.dev/time)
- Go by Example zu Zeitformatierung: [Go by Example: Time Formatting / Parsing](https://gobyexample.com/time-formatting-parsing)
