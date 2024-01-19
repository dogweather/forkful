---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Go: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Es ist wichtig zu überprüfen, ob ein Verzeichnis existiert, bevor Sie versuchen, mit ihm in Go zu interagieren. Diese Überprüfung verhindert lästige Fehler und Abstürze, die entstehen können, wenn wir versuchen, eine Datei zu lesen oder zu schreiben, die einfach nicht existiert.

## Wie geht's:

In Go verwenden wir den `os.Stat`-Funktion und `os.IsNotExist`-Funktion, um zu überprüfen, ob ein Verzeichnis existiert. Hier ist das Basisbeispiel:

```Go
package main

import (
  "fmt"
  "os"
)

func main() {
  
  _, err := os.Stat("/path/to/directory")

  // Überprüfen ob das Verzeichnis existiert.
  if os.IsNotExist(err) {
    fmt.Println("Das Verzeichnis existiert nicht.")
  } else {
    fmt.Println("Das Verzeichnis existiert.")
  }
}
```

Wenn Sie das ausführen, bekommen Sie eine der beiden Meldungen, abhängig davon, ob Ihr Verzeichnis existiert oder nicht.

## Tieftauchen:

Historischer Kontext: Der `os`-Paket in Go wurde entwickelt, um Systemaufrufe auf einfache Weise zu ermöglichen. Die `os.Stat`-Funktion ist Teil dieses Pakets und bietet Informationen über Dateisystem-Vorgänge.

Alternativen: Es gibt auch andere Möglichkeiten, die Existenz eines Verzeichnisses zu überprüfen. Man kann auch die `ioutil.ReadDir`-Funktion verwenden, die allerdings weniger gebräuchlich ist als `os.Stat`.

Implementierung: Der `os.Stat`-Funktion gibt ein `os.FileInfo`-Objekt und einen Fehler zurück. Das `os.FileInfo` enthält Metadaten zur Datei oder zum Verzeichnis. Wir ignorieren das `os.FileInfo`-Objekt und konzentrieren uns nur auf den Fehler. Wenn der Fehler auf `os.ErrExist` gesetzt ist, bedeutet das, dass das Verzeichnis nicht existiert.

## Siehe auch:
1. Go Standardbibliothek - os Paket: https://pkg.go.dev/os
2. Go os.Stat(): https://pkg.go.dev/os#Stat
3. Go os.IsNotExist(): https://pkg.go.dev/os#IsNotExist