---
title:                "Entfernen von Zeichen, die einem Muster entsprechen"
html_title:           "Go: Entfernen von Zeichen, die einem Muster entsprechen"
simple_title:         "Entfernen von Zeichen, die einem Muster entsprechen"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Löschen von Zeichen anhand eines bestimmten Musters ist eine häufig verwendete Technik unter Programmierern, um unerwünschte oder überflüssige Zeichen in einer Zeichenkette zu entfernen. Dies kann hilfreich sein, um bessere Suchergebnisse zu erzielen oder die Datenbereinigung zu vereinfachen.

## Wie geht's?

Das Löschen von Zeichen in Go ist einfach und unkompliziert. Es kann mit dem eingebauten "strings" Paket und der Funktion "Trim" durchgeführt werden. Hier ist ein Beispielcode:

```
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Dies ist ein Beispielstring"
    newStr := strings.Trim(str, "el")

    fmt.Println(newStr)
}
```

Die Ausgabe wird sein: "Dies ist ein Beispistring". Wie du sehen kannst, wurden alle Vorkommen von "el" aus der ursprünglichen Zeichenkette entfernt.

Es ist auch möglich, einen Zeichenbereich zu löschen, indem man den Start- und Endindex des Bereichs angibt, den man löschen möchte. Hier ist ein Beispiel:

```
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Dies ist ein Beispielstring"
    newStr := strings.Trim(str, "in")

    fmt.Println(newStr)
}
```

Die Ausgabe wird sein: "Dese st e Beispielstr". Beachte, dass nicht nur die einzelnen Zeichen "in" entfernt wurden, sondern auch der Bereich "in" innerhalb von "string".

## Tief eintauchen

Das Löschen von Zeichen anhand eines Musters ist keine neue Technik und wird schon seit langem von Programmierern verwendet. Alternative Methoden können das Verwenden von Regulären Ausdrücken oder das manuelle Durchlaufen der Zeichenkette sein.

In Go wird das Löschen von Zeichen durch die "trim" Funktion des "strings" Pakets ermöglicht, das Teil der Standardbibliothek ist. Diese Funktion wird intern eine effiziente Technik verwenden, um die Zeichen zu löschen.

## Siehe auch

- [Go strings Paket Dokumentation](https://golang.org/pkg/strings/)
- [Reguläre Ausdrücke in Go](https://golang.org/pkg/regexp/)