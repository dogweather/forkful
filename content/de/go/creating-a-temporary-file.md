---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Temporäre Dateien werden verwendet, um Daten kurzfristig zu speichern. Sie sind nützlich, wenn du mit großen Datenmengen arbeitest, die nicht dauerhaft im Arbeitsspeicher gehalten werden können.

## Wie es geht:

Erstelle eine temporäre Datei in Go mit der Funktion `ioutil.TempFile`. In diesem Beispiel erzeugen wir eine temporäre Datei, schreiben Daten hinein und lesen sie wieder aus:

```Go 
package main

import (
	"io/ioutil"
	"fmt"
)

func main() {
	tempFile, err := ioutil.TempFile("", "sample")

	if err != nil {
		panic(err)
	}
	
	defer tempFile.Close()

	byteSlice := []byte("Hallo Welt!")
	if _, err := tempFile.Write(byteSlice); err != nil {
		panic(err)
	}

	tempFile.Seek(0, 0)
	
	data, err := ioutil.ReadAll(tempFile)
	
	if err != nil {
		panic(err)
	}

	fmt.Println(string(data))
}
 ```

Der Ausgabetext wird sein:

```Go
Hallo Welt!
```

## Tiefgang:

Die Funktion `ioutil.TempFile` existiert bereits seit der Go Version 1.0. Sie war ursprünglich ein Teil des `io` Pakets, wurde aber später in das `io/ioutil` Paket verschoben.

Die Funktion erstellt automatisch eine Datei in dem vom Betriebssystem festgelegten temporären Verzeichnis, es sei denn, du gibst ein bestimmtes Verzeichnis an. Der Dateiname beginnt immer mit dem vorgegebenen Präfix.

Eine Alternative zur Erstellung temporärer Dateien ist die Nutzung von Speicherdatenstrukturen wie Slices oder Maps, aber diese halten Daten dauerhaft im Speicher, was nicht immer wünschenswert ist.

## Siehe Auch:

Für weitere Informationen zum Erstellen temporärer Dateien und zur Arbeit mit Dateien im Allgemeinen, schau dir diese Links an:

1. [Go by Example: Temporäre Dateien und Verzeichnisse](https://gobyexample.com/temporary-files-and-directories)
2. [Paket ioutil](https://golang.org/pkg/io/ioutil/#TempFile)