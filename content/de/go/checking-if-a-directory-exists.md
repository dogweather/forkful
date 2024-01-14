---
title:                "Go: Überprüfung, ob ein Verzeichnis besteht."
simple_title:         "Überprüfung, ob ein Verzeichnis besteht."
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum?

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Teil der Programmierung in Go. Dies ermöglicht es Entwicklern, sicherzustellen, dass Dateien und Ordner vorhanden sind, bevor sie darauf zugreifen oder sie bearbeiten. Dies minimiert potenzielle Fehler und hilft dabei, Anwendungen stabiler zu machen.

## Wie?

Um zu überprüfen, ob ein Verzeichnis in Go existiert, können wir die "os" Bibliothek verwenden. Wir können die Funktion "Stat" verwenden, um Informationen über eine Datei oder ein Verzeichnis abzurufen. Diese Funktion gibt eine Struktur zurück, die verschiedene Attribute wie Name, Größe und Zeitstempel enthält. Um zu überprüfen, ob ein Verzeichnis existiert, müssen wir auf das "IsDir" Feld der zurückgegebenen Struktur zugreifen. Wenn dieser Wert "true" ist, dann existiert das Verzeichnis.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Überprüfen, ob ein Verzeichnis existiert
    dirPath := "C:/Users/Downloads/"

    fileInfo, err := os.Stat(dirPath)
    if err != nil { 
        // Fehlerbehandlung 
        fmt.Println("Das Verzeichnis existiert nicht.")
    }

    if fileInfo.IsDir() {
        fmt.Println("Das Verzeichnis existiert.")
    }
}
```

Die Ausgabe wäre "Das Verzeichnis existiert." wenn das Verzeichnis existiert und "Das Verzeichnis existiert nicht." wenn es nicht existiert.

## Tiefer Einblick

Die "os" Bibliothek bietet auch die Funktion "Mkdir" an, mit der ein Verzeichnis erstellt werden kann, falls es nicht vorhanden ist. Wir können diese Funktion in Kombination mit der Überprüfung der Existenz eines Verzeichnisses verwenden, um sicherzustellen, dass wir immer auf ein vorhandenes Verzeichnis zugreifen. Eine andere nützliche Funktion ist "MkdirAll", die rekursiv Verzeichnisse erstellt, falls sie nicht existieren.

Ein weiterer wichtiger Punkt ist, dass wir immer die Fehlerbehandlung bei der Überprüfung der Existenz eines Verzeichnisses berücksichtigen sollten. Wenn ein Fehler auftritt, kann dies auf ein Problem mit dem Dateisystem oder auf andere Gründe zurückzuführen sein. Es ist wichtig, solche Fehler abzufangen und entsprechend zu reagieren.

## Siehe auch

- [Offizielle Dokumentation von Go zur "os" Bibliothek](https://golang.org/pkg/os/)
- [Tutorial zur Überprüfung von Datei und Verzeichnis in Go](https://www.digitalocean.com/community/tutorials/how-to-use-the-os-package-in-go)
- [Go Forum Diskussion über die Überprüfung der Existenz von Verzeichnissen](https://forum.golangbridge.org/t/how-can-i-check-a-directory-if-there-given-is-exist/2479)