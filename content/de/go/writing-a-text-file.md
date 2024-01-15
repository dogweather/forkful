---
title:                "Eine Textdatei schreiben"
html_title:           "Go: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Wenn du mit Go programmierst, wirst du früher oder später in der Lage sein, Textdateien zu erstellen und zu bearbeiten. Das kann hilfreich sein, um Daten zu speichern oder deine Anwendung mit externen Dateien zu integrieren.

## Wie geht's

Das Schreiben einer Textdatei in Go ist einfacher als du denkst. Zuerst musst du die "io/ioutil" Bibliothek importieren. Dann kannst du die Funktion "WriteFile" verwenden, um eine neue Datei zu erstellen und Text in sie zu schreiben. Schau dir das Beispiel unten an:

```Go
package main

import(
    "io/ioutil"
)

func main(){
    // Erstelle neue Datei namens "beispiel.txt"
    err := ioutil.WriteFile("beispiel.txt", []byte("Hallo Welt!"), 0644)
    if err != nil {
        // Fehlerbehandlung, falls das Schreiben fehlschlägt
        panic(err)
    } else {
        // Erfolgsmeldung, wenn alles geklappt hat
        fmt.Println("Datei erfolgreich erstellt und beschrieben!")
    }
}
```

Das obige Beispiel erstellt eine Datei namens "beispiel.txt" im gleichen Verzeichnis wie das Go-Programm und schreibt den Text "Hallo Welt!" in die Datei. Die Zahl "0644" gibt die Zugriffsrechte für die Datei an. Du kannst sie nach Belieben ändern.

## Tiefer tauchen

Um mehr über das Schreiben von Textdateien in Go zu erfahren, kannst du die Dokumentation der "io/ioutil" Bibliothek lesen. Dort findest du weitere nützliche Funktionen zum Lesen und Schreiben von Dateien. Außerdem gibt es andere Bibliotheken wie "os" und "bufio", die dir auch dabei helfen können, Textdateien zu manipulieren.

## Siehe auch

- Go Dokumentation von "io/ioutil": https://golang.org/pkg/io/ioutil/
- "os" Bibliothek: https://golang.org/pkg/os/
- "bufio" Bibliothek: https://golang.org/pkg/bufio/