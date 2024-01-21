---
title:                "Erstellung einer temporären Datei"
date:                  2024-01-20T17:40:19.353913-07:00
model:                 gpt-4-1106-preview
simple_title:         "Erstellung einer temporären Datei"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Temporäre Dateien sind kurzlebige Datenbehälter. Sie werden verwendet, um Daten zwischenzuspeichern, die während der Laufzeit eines Programms benötigt, aber nicht dauerhaft gespeichert werden sollen.

## So geht's:
Erstellung einer temporären Datei mit der Standardbibliothek:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"os"
)

func main() {
	tempFile, err := ioutil.TempFile("", "example")
	if err != nil {
		panic(err)
	}
	defer os.Remove(tempFile.Name()) // Aufräumen

	fmt.Println("Temporäre Datei erstellt:", tempFile.Name())

	// Schreibe etwas in die temporäre Datei
	content := []byte("Hallo Welt!")
	_, err = tempFile.Write(content)
	if err != nil {
		panic(err)
	}

	// Die Datei sollte jetzt Hallo Welt! enthalten.
}
```
Sample Output:
```
Temporäre Datei erstellt: /tmp/example123456
```

## Deep Dive
In Go werden temporäre Dateien häufig mit der `ioutil.TempFile`-Funktion erstellt. Diese Funktion erzeugt eine Datei mit einem eindeutigen Namen, um Kollisionen zu vermeiden. Der erste Parameter ist das Verzeichnis, der zweite das Präfix des Dateinamens.

Historisch gesehen war `ioutil.TempFile` Teil des `io/ioutil` Pakets, welches seit Go 1.16 als veraltet (deprecated) gilt. Die neuen Pfade sind `os` und `io` Pakete, spezifisch `os.CreateTemp` für temporäre Dateien.

Alternativ gibt es die Möglichkeit, mit `os.MkdirTemp` vorübergehende Verzeichnisse zu erstellen. Temporäre Dateien zu verwenden ist praktisch, um Ressourcenkonflikte und unnötige Disknutzung zu vermeiden. Sie eignen sich hervorragend für Testfälle, Zwischenspeicherungen oder Situationen, in denen Daten nicht dauerhaft relevant sind.

## See Also

- Go by Example – Temporary Files and Directories: https://gobyexample.com/temporary-files-and-directories
- Go Dokumentation für das os Paket: https://pkg.go.dev/os
- Go Dokumentation für das ioutil Paket (veraltet): https://pkg.go.dev/io/ioutil