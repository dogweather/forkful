---
title:    "Go: Textdatei schreiben"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum

Das Schreiben von Textdateien ist eine grundlegende Fähigkeit in der Programmierung, die es ermöglicht, Informationen auf einer promptgebundenen Oberfläche oder in einer Datei zu speichern. Dies kann nützlich sein, um beispielsweise ein Protokoll oder Konfigurationsdateien zu erstellen.

## Wie geht das?

Das folgende Beispiel zeigt, wie man in Go eine Textdatei erstellt und schreibt:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Öffne eine neue Textdatei mit dem Namen "neue_datei.txt"
    file, err := os.Create("neue_datei.txt")
    if err != nil {
        fmt.Println(err)
        return
    }
    defer file.Close()

    // Schreibe den Inhalt in die Datei
    _, err = file.WriteString("Hallo, Welt!")
    if err != nil {
        fmt.Println(err)
        return
    }

    // Überprüfe, ob der Schreibvorgang erfolgreich war
    fileInfo, err := file.Stat()
    if err != nil {
        fmt.Println(err)
        return
    }
    fmt.Printf("Die Datei wurde erfolgreich erstellt und ist %d Bytes groß.", fileInfo.Size())
}
```

Das obige Beispiel erstellt eine neue Textdatei mit dem Namen "neue_datei.txt" und schreibt den Inhalt "Hallo, Welt!" hinein. Dann wird die Dateigröße abgerufen und ausgegeben. Durch die Verwendung von `defer` wird sichergestellt, dass die Datei nach Abschluss der Funktion sorgfältig geschlossen wird, um Fehler zu vermeiden.

## Tiefergehende Information

Beim Schreiben einer Textdatei in Go gibt es einige wichtige Punkte zu beachten:

- Stelle immer sicher, dass die Datei nach dem Schreiben mit `defer` sorgfältig geschlossen wird, um mögliche Fehler zu vermeiden.
- Verwende `WriteString()` oder `Write()` zum Schreiben von Text in eine Datei. `Write()` akzeptiert nur `[]byte` als Eingabe, während `WriteString()` auch `string` akzeptiert.
- Überprüfe nach dem Schreiben die Größe der Datei mit `file.Stat()`, um sicherzustellen, dass der Vorgang erfolgreich war.

Mit diesen Informationen ausgestattet, kannst du nun problemlos Textdateien in Go erstellen und schreiben.

## Siehe auch

- [Go's os-Paket Dokumentation (in Deutsch)](https://golang.org/pkg/os/)
- [Tutorial: Arbeiten mit Dateien in Go](https://www.digitalocean.com/community/tutorials/how-to-work-with-files-in-go-de)
- [Golang Tutorium: Das Schreiben von Dateien](https://golangtutorials.blogspot.com/2011/06/writing-files-in-go.html)