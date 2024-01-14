---
title:    "Go: String in Großbuchstaben umwandeln"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren eines Strings ist ein häufiges Problem beim Programmieren in Go, insbesondere bei der Verarbeitung von Benutzereingaben. Es kann auch nützlich sein, wenn man Daten in einer bestimmten konventionellen Formatierung ausgeben möchte.

## So geht's

Die einfachste Methode, um einen String in Go zu kapitalisieren, ist die Verwendung der `strings` Bibliothek. Hier ist ein Beispielcode, der einen String in Großbuchstaben umwandelt:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "hallo welt"

    fmt.Println(strings.ToUpper(str))
}
```

Die Ausgabe für diesen Code wird wie folgt aussehen:

```
HALLO WELT
```

## Tieferer Einblick

In Go gibt es viele eingebaute Funktionen, die bei der Manipulation von Strings hilfreich sein können. Im obigen Beispiel haben wir `strings.ToUpper()` verwendet, aber es gibt auch `strings.ToLower()`, `strings.Title()` und `strings.TrimSpace()` - um nur einige zu nennen.

Außerdem ist es wichtig zu beachten, dass Go standardmäßig mit UTF-8 Strings arbeitet, was bedeutet, dass es Unicode-Unterstützung bietet. Dies ermöglicht eine einfache Handhabung von Strings in verschiedenen Sprachen und Zeichensätzen.

## Siehe auch

- [Offizielle Go Dokumentation zur `strings` Bibliothek](https://golang.org/pkg/strings/)
- [Tutorial zur Verwendung von Strings in Go](https://gobyexample.com/strings)
- [Weitere Informationen zu Unicode in Go](https://blog.golang.org/strings)

Vielen Dank fürs Lesen und viel Spaß beim Programmieren in Go!