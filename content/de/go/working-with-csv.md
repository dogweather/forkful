---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"

category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV steht für "Comma-Separated Values" – es sind einfache Textdateien, die Tabellendaten speichern. Programmierer nutzen CSVs, weil sie leicht zu lesen, zu schreiben und von Menschen und Maschinen verarbeitbar sind.

## How to:
```Go
package main

import (
	"encoding/csv"
	"fmt"
	"os"
	"strings"
)

func main() {
	// Ein CSV-String zum Testen
	csvContent := "Name,Alter,Beruf\nMax,30,Entwickler\nAnna,28,Designer"

	// String-Reader erstellen, um mit der CSV-Bibliothek zu verwenden
	reader := csv.NewReader(strings.NewReader(csvContent))

	// Alle Datensätze auslesen
	records, err := reader.ReadAll()
	if err != nil {
		panic(err)
	}

	// Datensätze ausgeben
	for _, record := range records {
		fmt.Println(record)
	}

	// In eine Datei schreiben
	file, err := os.Create("output.csv")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	writer := csv.NewWriter(file)
	defer writer.Flush()

	for _, record := range records {
		if err := writer.Write(record); err != nil {
			panic(err)
		}
	}
}
```

Ausgabe:
```
[Name Alter Beruf]
[Max 30 Entwickler]
[Anna 28 Designer]
```

## Deep Dive
CSV-Dateien haben eine lange Geschichte als einfache und unkomplizierte Art, Daten in Tabellenform zu speichern. Sie werden oft als Austauschformat verwendet, da die meisten Tabellenkalkulationsprogramme wie Microsoft Excel oder Google Sheets sie problemlos importieren und exportieren können. Alternativen zu CSV sind zum Beispiel XML und JSON; diese Formate sind jedoch komplexer in der Struktur. In Go erfolgt die Arbeit mit CSVs über das Paket `encoding/csv`, das effiziente Parsing- und Schreibmöglichkeiten bietet.

## See Also
- [Go-Dokumentation für das `encoding/csv` Paket](https://pkg.go.dev/encoding/csv)
- [CSV File Reading and Writing - Go Programming Language Wiki](https://github.com/golang/go/wiki/CSV)
