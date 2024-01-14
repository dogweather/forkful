---
title:    "Go: Ein temporäres Datei erstellen"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist ein häufiges Anliegen, wenn es um die Programmierung geht. Diese Dateien sind nützlich, um vorübergehend Daten zu speichern, die für eine bestimmte Aufgabe benötigt werden, aber nicht dauerhaft auf der Festplatte gespeichert werden müssen.

## Wie man es macht

In der Programmierung gibt es verschiedene Methoden, um eine temporäre Datei zu erstellen. Im Folgenden zeigen wir dir anhand einiger Beispiele, wie du das in Go umsetzen kannst.

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    // Beispiel 1: Verwenden von ioutil.TempFile()
    tmpFile, err := ioutil.TempFile("", "temp")
    if err != nil {
        panic(err)
    }
    defer os.Remove(tmpFile.Name())
    fmt.Println(tmpFile.Name())

    // Beispiel 2: Verwenden von os.CreateTemp()
    tmpFile2, err := os.CreateTemp("", "example")
    if err != nil {
        panic(err)
    }
    defer os.Remove(tmpFile2.Name())
    fmt.Println(tmpFile2.Name())
}

```

Die Ausgabe für beide Beispiele ist ähnlich: `C:\Users\User\AppData\Local\Temp\temp524196252` bzw. `C:\Users\User\AppData\Local\Temp\example961005604`.

Genauere Informationen zu den einzelnen Funktionen findest du in der Dokumentation von Go.

## Tiefer Einblick

Das Erstellen von temporären Dateien kann je nach Anwendung unterschiedliche Anforderungen haben. Glücklicherweise bietet Go auch dafür verschiedene Möglichkeiten. Du hast die Wahl zwischen `ioutil.TempFile()` und `os.CreateTemp()`, je nachdem, was deine Bedürfnisse sind. Beide Funktionen erstellen eine Datei im temporären Verzeichnis des Betriebssystems mit dem angegebenen Präfix und geben ein `*os.File` Objekt zurück, das für den Zugriff auf die Datei verwendet werden kann.

Eine wichtige Sache, die du beachten solltest, ist das Löschen der temporären Datei nach ihrer Verwendung. Dies kann entweder mit der Funktion `os.Remove()` oder `defer` erreicht werden, um sicherzustellen, dass die Datei am Ende des Programms gelöscht wird.

## Siehe auch

- Die offizielle Go-Dokumentation über `ioutil.TempFile()` und `os.CreateTemp()`: https://pkg.go.dev/io/ioutil#TempFile und https://pkg.go.dev/os#CreateTemp
- Ein Tutorial zum Erstellen von temporären Dateien in Go: https://golangbot.com/temporary-files/