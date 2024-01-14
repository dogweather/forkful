---
title:                "Go: Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum
Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Schritt für jede Go-Programmierung, da es die Funktionalität und Fehlerbehandlung verbessert.

# Wie
```Go
package main

import "fmt"
import "os"

func main() {
  // Erstelle ein Beispielverzeichnis
  os.Mkdir("beispielverzeichnis", 0755)

  // Überprüfe ob das Verzeichnis existiert
  if _, err := os.Stat("beispielverzeichnis"); os.IsNotExist(err) {
    fmt.Println("Das Verzeichnis existiert nicht")
  } else {
    fmt.Println("Das Verzeichnis existiert")
  }
}
```
Output: Das Verzeichnis existiert

# Deep Dive
Um ein Verzeichnis auf Existenz zu überprüfen, können wir die `os.Stat()` Funktion verwenden. Diese Funktion gibt eine `FileInfo` Struktur zurück, die Metadaten über die Datei oder das Verzeichnis enthält. Mithilfe der `os.IsNotExist()` Funktion können wir dann überprüfen, ob ein Fehler aufgetreten ist, der besagt, dass das Verzeichnis nicht existiert.

Der Vorteil dieser Methode ist, dass wir direkt auf Fehler reagieren können, anstatt eine vordefinierte Standardprozedur auszuführen.

# Siehe auch
- [Go Dokumentation zu os.Stat()](https://golang.org/pkg/os/#Stat)
- [Weitere Informationen zu Fehlerbehandlung in Go](https://blog.golang.org/defer-panic-and-recover)