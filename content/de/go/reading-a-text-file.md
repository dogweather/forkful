---
title:                "Textdatei einlesen"
date:                  2024-01-20T17:54:12.321098-07:00
model:                 gpt-4-1106-preview
simple_title:         "Textdatei einlesen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Textdateien lesen bedeutet, ihren Inhalt in ein Programm zu laden. Programmierer machen das, um Daten zu verarbeiten, Konfigurationen zu laden oder einfach Informationen zu nutzen.

## Wie geht das:
```go
package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
)

func main() {
	// Öffne Datei
	file, err := os.Open("beispiel.txt")
	if err != nil {
		log.Fatalf("Datei konnte nicht geöffnet werden: %v", err)
	}
	defer file.Close()

	// Lese Datei zeilenweise
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		fmt.Println(scanner.Text())
	}

	if err := scanner.Err(); err != nil {
		log.Fatalf("Fehler beim Lesen der Datei: %v", err)
	}
}
```
Beim Ausführen dieses Codes werden die Zeilen der Datei "beispiel.txt" in der Konsole ausgegeben.

## Tiefgang
Das Lesen von Textdateien ist ein alter Hut, aber essentiell. Schon in den frühesten Tagen der Computerei war das Einlesen von Daten grundlegend. Heutzutage gibt es viele Methoden, doch die im "Wie geht das:" Abschnitt ist einfach und effektiv.

Alternativen zum bufio.Scanner könnten die `ioutil.ReadFile` Funktion (vor Go 1.16) oder `os.ReadFile` (seit Go 1.16) sein, die die gesamte Datei auf einmal lesen. Abhängig von der Dateigröße kann das ineffizient sein.

Beim Lesen von großen Dateien ist es sinnvoll, Pufferung zu nutzen (wie mit bufio.Scanner), um nicht den Speicher zu sprengen. Außerdem ermöglicht die Pufferung, mit jeder neuen Zeile sofort zu arbeiten.

## Siehe auch
- Die offizielle Go-Dokumentation zum Lesen von Dateien: https://pkg.go.dev/bufio#Scanner
- Ein Tutorial zur Dateiverarbeitung in Go: https://gobyexample.com/reading-files
- Ein Blogpost über die Handhabung großer Dateien in Go: https://blog.golang.org/defer-panic-and-recover
