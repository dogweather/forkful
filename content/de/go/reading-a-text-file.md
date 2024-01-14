---
title:                "Go: Einen Textdatei lesen"
simple_title:         "Einen Textdatei lesen"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist eine grundlegende Fähigkeit in der Programmierung, die in vielen Anwendungen verwendet wird. Zum Beispiel können Sie mithilfe von Go-Code Textdateien lesen, um Informationen aus einer Datenbank zu extrahieren oder um Benutzereingaben zu verarbeiten.

## Wie man es macht

Das Lesen einer Textdatei in Go ist relativ einfach. Zunächst müssen Sie die `os`-Bibliothek importieren, um auf das Betriebssystem zugreifen zu können, sowie die `bufio`-Bibliothek, um das Lesen der Datei zu erleichtern. Dann verwenden Sie die `Open()`-Funktion, um die Datei zu öffnen, und `Scanner()` zum Lesen der Datei zeilenweise. Hier ist ein Beispiel:

```go
package main

import (
    "os"
    "bufio"
)

func main() {
    // Öffnen der Datei "textdatei.txt" zum Lesen
    file, err := os.Open("textdatei.txt")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    // Lesen der Datei Zeile für Zeile
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        // Ausgabe jeder Zeile der Datei
        fmt.Println(scanner.Text())
    }

    // Überprüfen auf Fehler beim Lesen
    if err := scanner.Err(); err != nil {
        panic(err)
    }
}
```

Die Ausgabe des obigen Codes wird jede Zeile der Datei "textdatei.txt" auf der Konsole ausgeben.

## Tiefer Einblick

Es gibt verschiedene Methoden, um Textdateien in Go zu lesen, je nach den Anforderungen Ihrer Anwendung. Zum Beispiel können Sie die `ioutil`-Bibliothek verwenden, um die gesamte Datei auf einmal zu lesen, oder die `ReadString()`-Funktion für eine bestimmte Zeichenfolge innerhalb der Datei. Sie können auch Dateien schreiben und andere Dateioperationen durchführen, indem Sie die `io`-Bibliothek verwenden.

## Siehe auch

- [Go-Dokumentation zur Datei-Eingabe/Ausgabe](https://golang.org/pkg/os/#File)
- [Go-Tutorial: Lesen und Schreiben von Dateien](https://www.golangprograms.com/go-language/write-to-file-in-go.html)
- [Ein Vergleich der Go-Lesemethoden für Dateien](https://medium.com/@elithrar/read-files-in-go-907df345f471)