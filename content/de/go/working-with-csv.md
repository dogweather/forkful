---
title:                "Arbeiten mit CSV"
html_title:           "Go: Arbeiten mit CSV"
simple_title:         "Arbeiten mit CSV"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-csv.md"
---

{{< edit_this_page >}}

Was & Warum?

"CSV" steht für "Comma Separated Values" und bezieht sich auf ein Dateiformat, welches zur Speicherung von tabellarischen Daten verwendet wird. Programmierer arbeiten häufig mit CSV-Dateien, da sie eine einfache und effiziente Möglichkeit bieten, Daten zu organisieren und zu analysieren.

Wie geht's:

Das Arbeiten mit CSV-Dateien ist in Go auch dank der integrierten Pakete "encoding/csv" und "bufio" sehr einfach. Hier ist ein Beispiel für das Lesen einer CSV-Datei und das Drucken der Daten auf der Konsole:

```
package main

import (
	"encoding/csv"
	"fmt"
	"os"
)

func main() {
	// Öffne die CSV-Datei
	file, err := os.Open("meine_daten.csv")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	// Erstelle einen neuen CSV-Reader
	reader := csv.NewReader(file)

	// Lies alle Zeilen der CSV-Datei
	records, err := reader.ReadAll()
	if err != nil {
		panic(err)
	}

	// Gehe durch jeden Datensatz und gib die Werte aus
	for _, record := range records {
		fmt.Println(record)
	}
}
```

Die Ausgabe dieses Programms würde wie folgt aussehen:

```
[Max Mustermann 25 männlich]
[Maria Musterfrau 32 weiblich]
[Paul Beispielmann 19 männlich]
```

Deep Dive:

Historisch gesehen wurde CSV in den 1970er Jahren entwickelt und diente ursprünglich der Übertragung von Daten zwischen verschiedenen Computersystemen. Heutzutage wird das Format häufig verwendet, um große Datenmengen zu organisieren und zu analysieren. Es gibt auch Alternativen zu CSV wie JSON oder XML, aber CSV bleibt aufgrund seiner Einfachheit und Verbreitung immer noch ein beliebtes Format.

Für diejenigen, die tiefer in das Arbeiten mit CSV-Dateien einsteigen möchten, gibt es viele nützliche Methoden und Funktionen in den "encoding/csv" und "bufio" Paketen. Diese ermöglichen es beispielsweise, bestimmte Teile der Daten zu ignorieren oder spezifische Datenformate zu definieren.

Siehe auch:

- Dokumentation zu "encoding/csv": https://golang.org/pkg/encoding/csv/
- Dokumentation zu "bufio": https://golang.org/pkg/bufio/