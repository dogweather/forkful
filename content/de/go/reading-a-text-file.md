---
title:    "Go: Einen Textdatei lesen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist eine wichtige Fähigkeit für jeden, der sich mit der Programmiersprache Go beschäftigt. Es ermöglicht uns, Daten von externen Quellen zu lesen und in unseren Code zu integrieren. In diesem Blogbeitrag werden wir uns genauer ansehen, wie wir Textdateien in Go lesen und verarbeiten können.

## Wie geht das?

Das Lesen einer Textdatei in Go ist relativ einfach und erfordert nur wenige Zeilen Code. Wir verwenden die `os.Open()` Funktion, um die Datei zu öffnen, und dann die `bufio` Bibliothek, um sie Zeile für Zeile zu lesen. Schauen wir uns ein Beispiel an:

```Go
package main

import (
    "fmt"
    "bufio"
    "os"
)

func main() {
    // Öffne die Textdatei
    file, err := os.Open("beispieldatei.txt")

    // Überprüfe auf Fehler
    if err != nil {
        fmt.Println(err)
    }

    // Versichern, dass die Datei am Ende wieder geschlossen wird
    defer file.Close()

    // Verwende den Scanner, um die Datei Zeile für Zeile zu lesen
    scanner := bufio.NewScanner(file)

    // Schleife über die Zeilen der Datei
    for scanner.Scan() {
        // Gib jede Zeile aus
        fmt.Println(scanner.Text())
    }

}
```

Wenn wir diese Datei ausführen, werden wir die gesamte Dateiinhalt Zeile für Zeile auf der Konsole sehen. Wir haben auch eine Fehlerprüfung eingebaut, um sicherzustellen, dass die Datei erfolgreich geöffnet wurde.

## Tiefer tauchen

Um noch genauer zu verstehen, wie das Lesen von Textdateien in Go funktioniert, müssen wir uns mit einigen wichtigen Konzepten vertraut machen. Erstens ist es wichtig, die Unterschiede zwischen der `os.Open()` Funktion und der `os.OpenFile()` Funktion zu verstehen. Während die `os.Open()` Funktion eine `File`-Struktur zurückgibt, die nur lesend ist, ermöglicht die `os.OpenFile()` Funktion es uns, die Datei zu öffnen und gleichzeitig verschiedene Berechtigungen festzulegen. 

Für eine detailliertere Erklärung dieser Konzepte und weitere nützliche Tipps beim Lesen von Textdateien in Go, empfehlen wir das Lesen der offiziellen Go-Dokumentation und das Ausprobieren von verschiedenen Code-Beispielen.

## Siehe auch

- Offizielle Go-Dokumentation: https://golang.org/pkg
- Tutorial zu Go File I/O: https://www.tutorialspoint.com/go/go_file_io.htm
- Eine Liste mit hilfreichen Go-Ressourcen: https://awesome-go.com/