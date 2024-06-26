---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:00:08.143027-07:00
description: "Wie geht das: Go bietet robuste Unterst\xFCtzung f\xFCr das Parsen von\
  \ Daten und Zeiten durch das `time` Paket. Der Schl\xFCssel liegt im Verst\xE4ndnis\
  \ von Gos\u2026"
lastmod: '2024-03-13T22:44:53.298332-06:00'
model: gpt-4-0125-preview
summary: "Go bietet robuste Unterst\xFCtzung f\xFCr das Parsen von Daten und Zeiten\
  \ durch das `time` Paket."
title: Ein Datum aus einem String interpretieren
weight: 30
---

## Wie geht das:
Go bietet robuste Unterstützung für das Parsen von Daten und Zeiten durch das `time` Paket. Der Schlüssel liegt im Verständnis von Gos Referenzdatumsformat: `Mon Jan 2 15:04:05 MST 2006`, welches Sie verwenden, um Go zu sagen, wie es den eingehenden String interpretieren soll. Hier ist ein schnelles Beispiel, um Ihnen den Einstieg zu erleichtern:

```go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Beispiel-Datumstring
	dateStr := "2023-04-12 14:45:00"
	
	// Definieren des Layouts/Formats des Eingabedatumstrings
	// Dieses Layout sagt Go, dass ein Jahr erwartet wird, gefolgt von einem Monat, 
	// dann einem Tag, einer Stunde, einer Minute und schließlich einer Sekunde
	layout := "2006-01-02 15:04:05"
	
	// Den Datumstring gemäß des Layouts parsen
	parsedDate, err := time.Parse(layout, dateStr)
	if err != nil {
		fmt.Println("Fehler beim Parsen des Datums:", err)
		return
	}
	
	// Das geparste Datum ausgeben
	fmt.Println("Geparstes Datum:", parsedDate)
}
```

Wenn Sie diesen Code ausführen, erhalten Sie:

```
Geparstes Datum: 2023-04-12 14:45:00 +0000 UTC
```

Beachten Sie, wie der `layout`-String die Werte des Referenzdatums verwendet, um das Format des Eingabestrings anzugeben. Passen Sie das `layout` an das Format Ihrer Eingabedaten an.

## Tiefere Betrachtung
Das Design von Gos Datums- und Zeitparsen ist einzigartig und nutzt ein spezifisches Referenzdatum (`Mon Jan 2 15:04:05 MST 2006`). Diese Herangehensweise, anstelle von konventionelleren Formatbezeichnern (wie `YYYY` für das Jahr), wurde wegen der Lesbarkeit und Benutzerfreundlichkeit gewählt, indem ein mehr beispielbasiertes Format verwendet wird.

Obwohl dies anfangs für Programmierer, die an andere Sprachen gewöhnt sind, ungewöhnlich erscheinen kann, finden es viele nach einer kurzen Anpassungszeit intuitiver. Für Anwendungen, die komplexere Datumsmanipulationen erfordern oder Formate, die nicht direkt von Gos `time` Paket unterstützt werden, können Drittanbieterbibliotheken wie `github.com/jinzhu/now` zusätzliche Funktionalitäten bieten. Allerdings sind für die Mehrheit der Standardanwendungen Gos eingebaute Fähigkeiten robust, leistungsfähig und idiomatisch und verkörpern die Go-Philosophie der Einfachheit und Klarheit.
