---
title:                "Go: Löschen von Zeichen mit passendem Muster"
simple_title:         "Löschen von Zeichen mit passendem Muster"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Hast du dich je gefragt, wie du in Go programmieren kannst, um bestimmte Zeichenfolgen aus einem Text zu löschen? Vielleicht möchtest du unerwünschte Zeichen entfernen oder eine bestimmte Formatierung beibehalten. In diesem Blogpost zeige ich dir, wie du mit der `strings` Bibliothek in Go Zeichenmuster löschen kannst.

## Wie

Um Zeichenmuster in Go zu löschen, verwendest du die `ReplaceAll` Funktion aus der `strings` Bibliothek. Diese Funktion akzeptiert drei Argumente: Die ursprüngliche Zeichenkette, das zu löschende Zeichenmuster und die Zeichenkette, die an dessen Stelle eingesetzt werden soll. Schauen wir uns ein Beispiel an:

```
package main

import (
    "fmt"
    "strings"
)

func main() {
    text := "Hello World"
    newText := strings.ReplaceAll(text, "o", "")
    fmt.Println(newText) // Hey Wlrld
}
```

In diesem Beispiel wird jedes Vorkommen des Buchstabens "o" in der Zeichenkette "Hello World" durch ein leeres Feld ersetzt. Das Ergebnis ist "Hey Wlrld". Du kannst auch mehrere Zeichenmuster gleichzeitig löschen, indem du sie in einem Array an `ReplaceAll` übergibst.

```
strings.ReplaceAll(text, []string{"o", "l"}, "")
// Das Ergebnis wäre "He Wrd"
```

## Deep Dive

Die `ReplaceAll` Funktion sucht bei jedem Aufruf die gesamte Zeichenkette nach dem angegebenen Muster ab. Wenn du jedoch nur das erste Vorkommen eines Zeichenmusters löschen möchtest, kannst du die `Replace` Funktion verwenden. Diese Funktion hat die gleichen Argumente wie `ReplaceAll`, aber löscht nur das erste Vorkommen des Musters.

Wenn du mehr Kontrolle über die zu löschenden Zeichenmuster haben möchtest, kannst du auch reguläre Ausdrücke mit der `Regexp` Bibliothek von Go verwenden. Diese ermöglicht es dir, komplexere Muster zu definieren und zu löschen. Ein Beispiel für das Verwenden von regulären Ausdrücken findest du in den "See Also" Links unten.

## Siehe auch

- Offizielle Go Dokumentation zur `strings` Bibliothek (https://golang.org/pkg/strings/)
- Beispiel für die Verwendung von regulären Ausdrücken in Go (https://gobyexample.com/regular-expressions)
- Weitere nützliche Funktionen der `strings` Bibliothek (https://www.digitalocean.com/community/tutorials/golang-manipulating-strings)