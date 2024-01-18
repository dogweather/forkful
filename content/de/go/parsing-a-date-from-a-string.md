---
title:                "Ein Datum aus einer Zeichenkette analysieren"
html_title:           "Go: Ein Datum aus einer Zeichenkette analysieren"
simple_title:         "Ein Datum aus einer Zeichenkette analysieren"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String ist ein häufiges Problem, das bei der Verarbeitung von Daten in Programmen auftritt. Es bezieht sich auf die Konvertierung eines Datumswerts, der als Zeichenkette (String) gespeichert ist, in ein datumsbezogenes Objekt. Programmierer machen dies, um das Datum in verschiedenen Formaten anzuzeigen oder um es für Berechnungen zu verwenden.

## So geht's:
Um ein Datum aus einem String in Go zu parsen, können Sie die Funktion ```time.Parse()``` verwenden. Hier ist ein Beispiel, wie Sie ein Datum aus einem String im Format "02.01.2006" parsen können und es dann in einem anderen Format ausgeben:

```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Parsen des Datums aus einem String
	dateString := "25.08.2020"
	date, _ := time.Parse("02.01.2006", dateString)

	// Ausgabe des Datums in einem anderen Format
	fmt.Println(date.Format("2006-01-02"))
}
```

Output:
```
2020-08-25
```

## Tiefere Einblicke:
Das Parsen von Daten aus Strings ist vor allem deshalb wichtig, weil Daten in verschiedenen Formaten gespeichert und verwendet werden können. Historisch gesehen waren viele Programmiersprachen, einschließlich Go, stark von der C-Syntax beeinflusst und verwendeten daher die Funktionen und Konventionen zur Formattierung von Datumsangaben, die vom Programmierer definiert wurden. Alternativ zum Parsen von Datumswerten aus Strings können Programmierer auch die Funktionen zur Formatierung nutzen, um die Datumsangabe in einem bestimmten Format anzuzeigen. Die genaue Implementierung des Datumparsens in Go kann in der Dokumentation der standardmäßigen Zeitpakete eingesehen werden.

## Siehe auch:
- [Package time in Go Documentation](https://golang.org/pkg/time/)
- [A Beginner's Guide to Parsing Data in Go](https://www.calhoun.io/parsing-json-responses-in-go/)
- [Working with Time and Dates in Go](https://devdactic.com/golang-dates/)