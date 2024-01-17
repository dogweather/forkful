---
title:                "Ein String in Kleinschreibung umwandeln"
html_title:           "Go: Ein String in Kleinschreibung umwandeln"
simple_title:         "Ein String in Kleinschreibung umwandeln"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Umwandlung von Zeichenketten in Kleinbuchstaben ist ein häufig verwendetes Konzept in der Programmierung. Dabei wird jede Großbuchstabe in der Zeichenkette in einen entsprechenden Kleinbuchstaben umgewandelt. Programmierer tun dies, um sicherzustellen, dass alle Eingaben auf die gleiche Weise behandelt werden und keine Fehler auftreten.

## How to:
In Go ist die Konvertierung einer Zeichenkette in Kleinbuchstaben sehr einfach. Hier ist ein Beispielcode, der eine Eingabeaufforderung ausgibt und die Eingabe in Kleinbuchstaben konvertiert:

```Go
package main

import "fmt"
import "strings"

func main() {
    fmt.Print("Gib einen Text ein: ")
    var input string
    fmt.Scanln(&input)
    
    output := strings.ToLower(input)
    fmt.Println("Dein Text in Kleinbuchstaben: " + output)
}
```

Beispiel Eingabe:

```Go
Gib einen Text ein: GO is Awesome!
```

Beispiel Ausgabe:

```Go
Dein Text in Kleinbuchstaben: go is awesome!
```

## Deep Dive:
Die Umwandlung von Zeichenketten in Kleinbuchstaben ist kein neues Konzept und wurde schon seit langer Zeit in verschiedenen Programmiersprachen implementiert. In der Vergangenheit gab es verschiedene Methoden, dies zu erreichen, aber in modernen Programmiersprachen wie Go ist es in der Standardbibliothek enthalten.

Es gibt auch alternative Methoden, um eine Zeichenkette in Kleinbuchstaben umzuwandeln, wie z.B. die Verwendung von regulären Ausdrücken oder manueller Iteration durch jedes Zeichen. In der Regel ist die Verwendung von Standardbibliotheksfunktionen jedoch effizienter und zuverlässiger.

Die Implementierungsdetails der Konvertierung von Zeichenketten in Kleinbuchstaben hängen von der verwendeten Programmiersprache ab. In Go wird die Funktion ```strings.ToLower()``` verwendet, die jeden Buchstaben in der Zeichenkette mit Hilfe der Unicode-Tabelle in den entsprechenden Kleinbuchstaben umwandelt.

## Siehe auch:
- [Go Strings Package Documentation](https://golang.org/pkg/strings/)
- [Wikipedia Artikel über Groß- und Kleinschreibung](https://de.wikipedia.org/wiki/Gro%C3%9Fschreibung_und_Kleinschreibung)