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

# Was & Warum?

Das Schreiben einer Textdatei ist für Programmierer eine übliche Aufgabe, vor allem wenn es darum geht, Daten zu speichern oder zu exportieren. Textdateien enthalten einfachen, lesbaren Text, der in jedem Texteditor geöffnet und bearbeitet werden kann. Programmierer nutzen Textdateien, um zum Beispiel Konfigurationen zu speichern oder Ergebnisse von Programmen zu exportieren.

# Wie geht's?

Um eine Textdatei in Go zu schreiben, nutzen wir die Package `io/ioutil` und die Funktion `WriteFile()`. Hier ist ein Beispielcode für das Schreiben einer einfachen Textdatei mit dem Inhalt "Hello, World!":

```
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    content := []byte("Hello, World!")
    err := ioutil.WriteFile("hello.txt", content, 0644)
    if err != nil {
        fmt.Println(err)
    }
}
```

Die Funktion `WriteFile()` nimmt drei Parameter: den Namen der Datei, den Inhalt der Datei als `[]byte` und die Berechtigungen für die Datei. In diesem Beispiel haben wir die Berechtigungen `0644` gewählt, was bedeutet, dass die Datei für den Besitzer lesbar und schreibbar ist, aber für alle anderen nur lesbar.

Nach Ausführung des Codes wird eine Datei mit dem Namen "hello.txt" erstellt und der Inhalt "Hello, World!" in die Datei geschrieben.

# Tiefer Einblick

Schreiben von Textdateien ist eine gängige Aufgabe in der Programmierung und kann auf verschiedene Arten erledigt werden. In Go gibt es mehrere Libraries, die das Schreiben von Textdateien ermöglichen, wie zum Beispiel `bufio` und `os`. Allerdings bietet `io/ioutil` eine einfache und effiziente Möglichkeit, Textdateien zu schreiben.

Zusätzlich zur Funktion `WriteFile()` gibt es in `io/ioutil` auch die Funktionen `ReadFile()` und `AppendFile()`, die das Lesen und Anhängen von Textdateien ermöglichen.

# Siehe Auch

Für weitere Informationen zum Schreiben von Textdateien in Go, schau dir die offizielle Dokumentation an: https://golang.org/pkg/io/ioutil/

Wenn du mehr über die Go Packages `os` und `bufio` erfahren möchtest, findest du hier weitere Infos: https://gobyexample.com/reading-files und https://gobyexample.com/writing-files.