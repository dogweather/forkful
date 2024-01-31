---
title:                "Schreiben auf Standardfehler"
date:                  2024-01-19
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Standardfehler (stderr) ist ein Ausgabekanal, den Programme nutzen, um Fehlermeldungen und Diagnoseinformationen zu senden. Programmierer verwenden stderr, um Fehler von normalen Ausgaben zu trennen, was beim Debuggen und Protokollieren hilft.

## Wie geht das?
Verwendung von `os.Stderr` zum Schreiben:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	if _, err := os.Stat("nichtexistent.datei"); err != nil {
		fmt.Fprintf(os.Stderr, "Fehler gefunden: %v\n", err)
	}
}
```

Beispielhafte Ausgabe:

```plaintext
Fehler gefunden: stat nichtexistent.datei: no such file or directory
```

## Tiefere Einblicke
Historisch gesehen folgt stderr der Unix-Konvention, drei primäre Datenströme zu verwenden: Standard Input (stdin), Standard Output (stdout) und Standard Error (stderr). Alternativen für stderr sind das Schreiben in Log-Dateien oder die Verwendung von Logging-Frameworks. Die Implementierung in Go erfolgt über das `os` Paket, das plattformübergreifend Zugriff auf Systemfunktionen bietet, einschließlich File Descriptors, welche für stderr typisch die Nummer 2 haben.

## Siehe auch
- Go-Dokumentation für das `fmt` Paket: https://pkg.go.dev/fmt
- Go-Dokumentation für das `os` Paket: https://pkg.go.dev/os
- Artikel zu Unix-Datei-Streams: https://en.wikipedia.org/wiki/Standard_streams
