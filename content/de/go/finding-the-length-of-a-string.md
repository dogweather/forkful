---
title:                "Die Länge eines Strings finden"
html_title:           "Go: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge einer Zeichenkette zu finden, bedeutet, die Anzahl der Zeichen in der Zeichenkette zu bestimmen. Programmierer tun dies, um beispielsweise die Größe eines Textfeldes zu bestimmen oder zu überprüfen, ob die Eingabe vom Nutzer die erwartete Länge hat.

## Wie geht's?
Das Finden der Länge einer Zeichenkette in Go ist einfach. Verwenden Sie einfach die Funktion `len()`, die die Länge des übergebenen Strings zurückgibt. Hier ist ein Beispiel:

```
package main

import "fmt"

func main() {
    str := "Guten Tag!"
    
    fmt.Printf("Die Länge der Zeichenkette '%s' ist %d", str, len(str))
}
```

Die Ausgabe wäre: `Die Länge der Zeichenkette 'Guten Tag!' ist 10`.

## Tiefere Einblicke
Das Finden der Länge einer Zeichenkette ist keine neue Funktion. Es ist seit den Anfängen der Programmierung ein wichtiges Werkzeug. Früher wurde dies oft manuell durch das Zählen der Zeichen durchgeführt, aber mit der Entwicklung von Programmiersprachen gibt es nun Standardfunktionen wie `len()`. Alternativ zur `len()`-Funktion gibt es auch andere Methoden, um die Länge einer Zeichenkette zu bestimmen, wie z.B. `utf8.RuneCountInString()`, die die Anzahl der UTF-8 Codierungseinheiten zurückgibt.

## Siehe auch
- [Dokumentation zur Go `len()` Funktion](https://golang.org/pkg/builtin/#len)
- [Eine kurze Geschichte der Zeichenkettenmanipulation](http://web.psung.name/hstripping/ch01s06.xhtml)
- [Weitere Details zur UTF-8 Codierung](https://www.w3.org/International/questions/qa-utf8-bom.de)