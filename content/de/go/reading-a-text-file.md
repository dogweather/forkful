---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was und Warum?

Ein Textfile zu lesen bedeutet, den Inhalt eines solchen Files programmatisch auszulesen und im Programm zu verarbeiten. Programmierer machen das, um Daten zu analysieren, zu manipulieren oder zu speichern.

## Und so geht's:

Ein einfaches Beispiel in Go, wie man ein File liest:

```Go
package main

import (
	"fmt"
	"io/ioutil"
)

func main() {
	data, err := ioutil.ReadFile("dateiname.txt")
	if err != nil {
		fmt.Println("Datei konnte nicht gelesen werden", err)
		return
	}
	
	fmt.Println("Datei wurde erfolgreich gelesen:")
	fmt.Println(string(data))
}
```
Wenn Ihr "dateiname.txt" ersetzt durch den Namen eines existierenden Files, wird der Inhalt dieses Files auf der Konsole ausgegeben. 

## Tiefere Erkenntnisse

Historisch gesehen, kommen File-Operationen bereits in den frühesten Computersystemen vor und sind integraler Bestandteil fast aller Programmiersprachen. In Go gibt es neben der vorgestellten Methode `ioutil.ReadFile()` auch die Möglichkeit, das `os`-Paket zu verwenden. Besonders für größere Files ist der Einsatz von `bufio.Scanner` sinnvoll, da hier nicht das komplette File in den Speicher geladen wird. 

Über Details der Implementierung von `ioutil.ReadFile` hinaus verwendet diese Funktion `os.Open` zum Öffnen des Files und `ioutil.ReadAll` zum Lesen des Files. 

## Weiterführendes

Hier sind einige nützliche Links, wenn Sie mehr über das Lesen von Dateien in Go lernen möchten:
- Offizielle Dokumentation: https://golang.org/doc/
- Einführung in File IO in Go: https://gobyexample.com/reading-files
- Go's `ioutil` Paket: https://golang.org/pkg/io/ioutil/