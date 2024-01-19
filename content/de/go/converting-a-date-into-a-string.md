---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was und Warum?

Das Umwandeln eines Datums in einen String bedeutet, ein Date-Objekt in eine lesbare Zeichenkadette umzuwandeln. Programmierer machen das, um Termininformationen in Nachrichten, Protokollen und Benutzeroberflächen anzuzeigen.

## So geht's:

Hier zeigen wir, wie man ein aktuelles Datum in einen String umwandelt, und dann ein spezifisches Format dafür verwendet.

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	t := time.Now()
	fmt.Println("Aktuelles Datum:", t.Format(time.RFC3339))
}
```
Führen Sie das Programm aus und Sie werden sehen, dass das aktuelle Datum und die Uhrzeit im RFC3339-Format angezeigt werden.

## Tiefer Tauchen:

Diese spezifische Methode, Daten in Strings umzuwandeln, wurde in Go eingeführt, um den Datumszeit-Handling-Prozess zu vereinfachen. In anderen Sprachen wie Java oder Python passiert das ziemlich anders. 

Außerdem gibt es in Go mehrere Datumsformat-Layouts, die Sie verwenden können. RFC3339 ist nur eines davon. Sie können auch eigene Layouts erstellen, um spezifische Anforderungen an das Datumsformat zu erfüllen.

Im Hinblick auf die Implementierungsdetails verwenden Go und das "time" -Paket eine Layouts-Konstante (wie time.RFC3339), das als Muster für das Erstellen des erforderlichen Formats fungiert. Im Hintergrund verwendet Go eine eigene Layoutdefinition anhand von "Mon Jan 2 15:04:05 MST 2006".

## Siehe Auch:

Besuchen Sie die offizielle Dokumentation für weitere Informationen:

1. Go Time Package: https://golang.org/pkg/time/
2. Zeit- und Datum-Formatierung in Go: https://gobyexample.com/time-formatting-parsing