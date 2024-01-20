---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:56:33.993516-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
"Was & Warum?"
Beim Prüfen, ob ein Verzeichnis existiert, kontrollieren wir, ob ein bestimmter Ordner im Dateisystem vorhanden ist. Wir machen das, um Fehler zu vermeiden, etwa das Schreiben in ein nicht vorhandenes Verzeichnis.

## How to:
"So geht's:"
Hier ein einfaches Beispiel, wie man in Go überprüft, ob ein Verzeichnis existiert:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	dir := "/path/to/your/directory"
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		fmt.Printf("Das Verzeichnis %s existiert nicht.\n", dir)
	} else {
		fmt.Printf("Das Verzeichnis %s existiert.\n", dir)
	}
}
```

Sample Output:

```
Das Verzeichnis /path/to/your/directory existiert nicht.
```
oder

```
Das Verzeichnis /path/to/your/directory existiert.
```
abhängig davon, ob das Verzeichnis existiert.

## Deep Dive:
"Tiefgang:"
Früher wurden in vielen Programmiersprachen Routinen genutzt, die teils umständlich klärten, ob ein Verzeichnis existiert. In Go nutzen wir die `os.Stat()` Funktion, um Datei- oder Verzeichnisdetails zu erhalten. Ein Fehlerwert, zurückgegeben von `os.Stat()`, gekoppelt mit `os.IsNotExist(err)`, zeigt, ob ein Verzeichnis nicht existiert. Alternativen sind das Erstellen des Verzeichnisses mit `os.Mkdir()` oder `os.MkdirAll()`, die es auch prüfen. Bei `os.MkdirAll()` wird das Verzeichnis erstellt, auch wenn übergeordnete Pfade fehlen.

## See Also:
"Siehe auch:"
Weitere nützliche Ressourcen zu diesem Thema:

- Go by Example: Reading Files: https://gobyexample.com/reading-files
- Go Dokumentation zum os Paket: https://pkg.go.dev/os
- Blogpost zu File Operations in Go: https://blog.golang.org/2011/06/07/using-go-directories.html