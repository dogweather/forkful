---
title:                "Das Lesen einer Textdatei"
html_title:           "Go: Das Lesen einer Textdatei"
simple_title:         "Das Lesen einer Textdatei"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Du hast bereits gelernt, wie man grundlegende Programme in Go schreibt und möchtest nun lernen, wie man mit Textdateien arbeitet. Das Lesen von Textdateien ist ein wesentlicher Bestandteil der Programmierung und wird dir helfen, komplexere Projekte zu erstellen.

## Wie Geht Man Vor
Das Lesen einer Textdatei in Go ist ein einfacher Vorgang. Zunächst musst du die `os`-Bibliothek importieren, um auf das Dateisystem zuzugreifen. Dann kannst du die `Open()`-Funktion verwenden, um eine Datei zu öffnen und ein sogenanntes `File`-Objekt zurückzugeben.

```Go
package main 

import (
    "os"
)

func main() {
    // Öffne die Datei
    file, err := os.Open("textdatei.txt")
    // Behandele den Error-Fall
    if err != nil {
        panic(err)
    }
    // Lies die Datei Zeile für Zeile
    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }
    // Schließe die Datei
    defer file.Close()
}
```

In unserem Beispiel verwenden wir die `Open()`-Funktion, um eine Datei namens "textdatei.txt" zu öffnen. Dann durchlaufen wir mit einer `for`-Schleife jede Zeile der Datei und geben sie aus. Zum Schluss schließen wir die Datei mit der `Close()`-Funktion, um sicherzustellen, dass sie nicht länger zum Lesen verfügbar ist.

## Tiefer Einblick
Das `File`-Objekt, das von der `Open()`-Funktion zurückgegeben wird, verfügt über verschiedene Methoden, um Informationen über die Datei zu erhalten, z. B. die Größe und den Dateinamen. Außerdem gibt es auch erweiterte Methoden, um bestimmte Abschnitte der Datei zu lesen oder zu schreiben. Es ist wichtig, dass du die Datei immer schließt, wenn du fertig bist, um sicherzustellen, dass sie nicht unbeabsichtigt geändert wird.

## Siehe Auch
- [Die offizielle Go-Dokumentation zu Dateiein-/ausgabe](https://golang.org/pkg/os/)
- [Eine Anleitung zum Schreiben von Textdateien in Go](https://gobyexample.com/writing-files)
- [GitHub-Repository mit praktischen Beispielprogrammen für das Lesen von Dateien in Go](https://github.com/avelino/awesome-go#writing-files)