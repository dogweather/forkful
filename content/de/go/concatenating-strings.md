---
title:                "Go: Zusammenfügen von Zeichenketten"
simple_title:         "Zusammenfügen von Zeichenketten"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Zusammenfügen von Strings ist eine häufige Aufgabe beim Programmieren. Es ermöglicht es, mehrere Texte oder Zeichenketten zu einer neuen zusammenzufügen. In diesem Blogbeitrag werden wir uns anschauen, wie man dieses Konzept in der Programmiersprache Go umsetzen kann.

## Wie geht man vor?

Um Strings in Go zusammenzufügen, gibt es verschiedene Möglichkeiten. Eine davon ist die Verwendung des "+" Operators. Hier ist ein Beispielcode, der zwei Strings zusammenfügt und das Ergebnis ausgibt:

```Go
package main

import "fmt"

func main() {
    text1 := "Hallo"
    text2 := "Welt"
    result := text1 + text2
    fmt.Println(result)
}
```

Die Ausgabe dieses Codes wäre "HalloWelt".

Eine andere Möglichkeit ist die Verwendung der "fmt.Sprintf" Funktion. Hier ein Beispiel, wie man drei Strings zusammenfügt:

```Go
package main

import "fmt"

func main() {
    text1 := "Hallo"
    text2 := " "
    text3 := "Welt"
    result := fmt.Sprintf("%s%s%s", text1, text2, text3)
    fmt.Println(result)
}
```

Die Ausgabe dieses Codes wäre ebenfalls "Hallo Welt".

## Tiefere Einblicke

Beim Zusammensetzen von Strings in Go ist es wichtig, die Performance im Auge zu behalten. Da Go eine statisch typisierte Sprache ist, muss bei der Verwendung des "+" Operators darauf geachtet werden, dass die Datentypen der zu verknüpfenden Variablen übereinstimmen. Andernfalls kann es zu Fehlern oder Ineffizienzen kommen.

Ein weiterer Punkt ist die Verwendung von "strings.Join" anstelle von "fmt.Sprintf". Diese Funktion ist effizienter, da sie einen Puffer für die verknüpften Strings verwendet, was die Anzahl der Allokationen reduziert und somit die Performance verbessert.

## Siehe auch

- [Das offizielle Go-Tutorial zu Strings](https://tour.golang.org/basics/1)
- [Ein ausführlicherer Artikel über Strings in Go](https://blog.golang.org/strings)
- [Die Dokumentation zu "fmt.Sprintf"](https://golang.org/pkg/fmt/#Sprintf)