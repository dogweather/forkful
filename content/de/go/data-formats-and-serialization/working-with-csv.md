---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:39.938643-07:00
description: "Wie geht das: Mit CSV-Dateien in Go zu arbeiten, ist dank der Standardbibliothek\
  \ `encoding/csv` unkompliziert. Im Folgenden finden Sie eine Einf\xFChrung in\u2026"
lastmod: '2024-03-13T22:44:53.312197-06:00'
model: gpt-4-0125-preview
summary: Mit CSV-Dateien in Go zu arbeiten, ist dank der Standardbibliothek `encoding/csv`
  unkompliziert.
title: Arbeiten mit CSV
weight: 37
---

## Wie geht das:
Mit CSV-Dateien in Go zu arbeiten, ist dank der Standardbibliothek `encoding/csv` unkompliziert. Im Folgenden finden Sie eine Einführung in das Lesen und Schreiben von CSV-Dateien.

### Ein CSV-Datei lesen
Um aus einer CSV-Datei zu lesen, öffnen Sie zuerst die Datei mit `os.Open` und erstellen dann einen neuen CSV-Leser mit `csv.NewReader`.

```go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
)
func main() {
    file, err := os.Open("data.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    reader := csv.NewReader(file)
    records, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    for _, record := range records {
        fmt.Println(record)
    }
}
```

Dieser Codeausschnitt liest alle Datensätze aus `data.csv` und druckt sie aus. Jeder Datensatz ist ein Slice von Feldern.

### In eine CSV-Datei schreiben
Zum Schreiben verwenden Sie `csv.NewWriter` und `writer.WriteAll` oder `writer.Write` für das Schreiben von mehreren oder einzelnen CSV-Datensätzen.

```go
package main

import (
    "encoding/csv"
    "os"
)

func main() {
    file, err := os.Create("output.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := csv.NewWriter(file)
    defer writer.Flush()

    records := [][]string{
        {"Name", "Age", "City"},
        {"John Doe", "30", "New York"},
        {"Jane Doe", "27", "Los Angeles"},
    }

    if err := writer.WriteAll(records); err != nil {
        panic(err)
    }
}
```

Dies erstellt eine Datei namens `output.csv` mit den bereitgestellten Datensätzen. Denken Sie immer daran, den Schreiber zu flushen, um sicherzustellen, dass alle gepufferten Daten in die Datei geschrieben werden.

## Tiefergehend
Das Go-`encoding/csv`-Paket bietet robuste Unterstützung für das Lesen und Schreiben von CSV-Dateien, ist jedoch mit Einfachheit im Sinn konzipiert, was bedeutet, dass es komplexere Szenarien wie die automatische Erkennung von Trennzeichen, den Umgang mit Anführungszeichen oder eingebettete Zeilenumbrüche in Feldern ohne manuelle Handhabung nicht bewältigt.

Historisch gesehen war die CSV-Handhabung in Programmiersprachen oft umständlich aufgrund dieser Komplexitäten, aber Go's Standardbibliothek abstrahiert viele dieser Probleme, was Entwicklern erlaubt, mit CSV-Daten mit relativer Leichtigkeit zu arbeiten. Für komplexere CSV-Manipulationen könnten jedoch Drittanbieterbibliotheken wie `gocsv` oder manuelles Parsen notwendig sein.

Ein bemerkenswerter Aspekt des Go-`csv`-Pakets ist seine Unterstützung für die Spezifizierung benutzerdefinierter Kommas (Trennzeichen), was es ermöglicht, nahtlos mit Varianten von CSV-Dateien, wie durch Tabulatoren getrennte Werte (TSV), zu arbeiten. Wenn man jedoch mit hochgradig unregelmäßigen oder nicht standardmäßigen CSV-Dateien zu tun hat, könnten sich Go-Programmierer gezwungen sehen, die bestehenden CSV-Leser- oder Schreiberimplementierungen zu erweitern.

Obwohl die CSV-Handhabungsfähigkeiten von Go für allgemeine Zwecke robust sind, könnten Programmierer für Anwendungen, die intensive Datenmanipulation erfordern, wie Data Science oder komplexe Datentransformationsaufgaben, sich dedizierten Datenverarbeitungspaketen oder sogar anderen Sprachen zuwenden, die besser für diese Aufgaben geeignet sind, wie Python mit seiner `pandas`-Bibliothek. Dennoch, für unkomplizierte CSV-Lese- und Schreiboperationen, sticht die Standardbibliothek von Go durch ihre Effizienz und Einfachheit hervor.
