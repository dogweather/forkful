---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Go: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Die Löschung von Zeichen, die einem bestimmten Muster entsprechen, kann in verschiedenen Fällen nützlich sein, wie zum Beispiel beim Bereinigen von Texten oder der Datenmanipulation. Mit Go ist das Löschen solcher Zeichen schnell und effizient möglich.

## Wie geht's

Die Funktion `Trim()` aus dem Paket `strings` wird verwendet, um Zeichen an Anfang und Ende eines Strings zu löschen. Für das Löschen von Zeichen an beliebigen Positionen gibt es die Funktion `ReplaceAll()` aus demselben Paket.

Beispielcode für das Löschen von Zeichen am Anfang und Ende eines Strings:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "    Hallo Welt!    "
    fmt.Println(strings.Trim(s, " "))
    // Ausgabe: "Hallo Welt!"

    fmt.Println(strings.Trim(s, "! "))
    // Ausgabe: "    Hallo Welt"
}
```
Beispielcode für das Löschen von Zeichen an beliebigen Positionen im String:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    s := "Hallo Welt!"
    fmt.Println(strings.ReplaceAll(s, "e", ""))
    // Ausgabe: "Hallo Wlt!"

    fmt.Println(strings.ReplaceAll(s, "l", ""))
    // Ausgabe: "Hao Wet!"
}
```

## Tief tauchen

Die Funktionen `Trim()` und `ReplaceAll()` arbeiten mit einem Muster, das angegeben wird, um die zu löschenden Zeichen zu identifizieren. Dieses Muster kann entweder als einzelnes Zeichen oder als Zeichenfolge angegeben werden.

Bei der Verwendung von `Trim()` werden Zeichen am Anfang und Ende des Strings gelöscht, bis kein Zeichen im Muster mehr gefunden wird. Dies kann nützlich sein, um Leerzeichen oder Zeilenumbrüche zu entfernen.

`ReplaceAll()` hingegen ersetzt alle Vorkommnisse des Musters im String mit einer leeren Zeichenfolge. Dadurch können beliebige Zeichen an allen Positionen im String gelöscht werden.

Es ist auch möglich, das Muster mit regulären Ausdrücken zu definieren, um noch mehr Flexibilität bei der Löschung von Zeichen zu haben.

## Siehe auch

- [The Go Programming Language](https://golang.org/)
- [Official Go Documentation](https://golang.org/doc/)
- [How to Trim and Replace Strings in Go](https://www.digitalocean.com/community/tutorials/how-to-trim-and-replace-strings-in-go)
- [Go By Example: String Functions](https://gobyexample.com/string-functions)