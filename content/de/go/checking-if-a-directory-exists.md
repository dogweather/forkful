---
title:    "Go: Überprüfung der Existenz eines Verzeichnisses"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

Das Überprüfen, ob ein Verzeichnis vorhanden ist, ist ein wichtiger Teil der Datei- und Ordnerverwaltung in Go-Programmen. Es kann verwendet werden, um sicherzustellen, dass bestimmte Aktionen nur auf vorhandenen Verzeichnissen durchgeführt werden und um Fehler in der Programmausführung zu vermeiden.

# Wie geht man vor

Um die Existenz eines Verzeichnisses in Go zu überprüfen, können wir die Funktion `os.Stat()` verwenden. Diese Funktion akzeptiert einen Dateipfad als Argument und gibt eine `os.FileInfo` Struktur zurück, die Informationen über die Datei oder das Verzeichnis enthält.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Überprüfe ob das Verzeichnis "beispiel" vorhanden ist
    file, err := os.Stat("beispiel")

    // Überprüfe ob ein Fehler aufgetreten ist
    if err != nil {
        // Wenn der Fehler eine Nicht-Vorhanden Fehler ist, existiert das Verzeichnis nicht
        if os.IsNotExist(err) {
            fmt.Println("Das Verzeichnis 'beispiel' existiert nicht.")
            return
        }
        // Ansonsten ist ein unerwarteter Fehler aufgetreten
        panic(err)
    }

    // Überprüfe ob es sich bei dem Ergebnis tatsächlich um ein Verzeichnis handelt
    if file.Mode().IsDir() {
        fmt.Println("Das Verzeichnis 'beispiel' existiert.")
    } else {
        fmt.Println("Der Pfad ist kein Verzeichnis.")
    }
}
```

Ausgabe:

```
Das Verzeichnis 'beispiel' existiert.
```

# Tiefergehende Informationen

Die `os.FileInfo` Struktur, die von der `os.Stat()` Funktion zurückgegeben wird, enthält verschiedene Methoden und Eigenschaften, die nützlich sein können, um mehr Informationen über das Verzeichnis zu erhalten.

Zum Beispiel können wir mit der `Name()` Methode den Namen des Verzeichnisses abrufen oder mit der `Size()` Methode die Größe des Verzeichnisses in Bytes erhalten.

Darüber hinaus gibt es auch eine `ModTime()` Methode, die uns das Datum und die Uhrzeit der letzten Änderung des Verzeichnisses gibt. Diese Informationen können hilfreich sein, um zu überprüfen, ob das Verzeichnis aktualisiert wurde oder nicht.

# Siehe auch

- [Go-Pakete zum Arbeiten mit Dateien und Verzeichnissen](https://golang.org/pkg/os/)
- [Offizielle Go-Dokumentation zur os.Stat() Funktion](https://golang.org/pkg/os/#Stat)