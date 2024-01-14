---
title:    "Go: String auf Großbuchstaben umwandeln."
keywords: ["Go"]
---

{{< edit_this_page >}}

# Warum

Es gibt viele Situationen in der Programmierung, in denen es notwendig ist, Strings zu formatieren und zu bearbeiten. Eine häufige Aufgabe ist das Großschreiben von Strings, sei es für die Ausgabe in einem konsolenbasierten Programm oder für die Verwendung in einer Datenbankabfrage. In diesem Blog-Beitrag werde ich erklären, wie man dies mit der Go-Programmiersprache erreichen kann.

# Wie geht man vor?

Um Strings in Go zu kapitalisieren, gibt es verschiedene Ansätze. Der einfachste Weg ist die Verwendung der `strings`-Bibliothek, die bereits eine Funktion `ToUpper` zur Verfügung stellt.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "hallo"
    upperStr := strings.ToUpper(str)

    fmt.Println(upperStr) // Ausgabe: HALLO
}
```

In diesem Beispiel wird die Funktion `ToUpper` auf den String `str` angewendet und das Ergebnis in der Variable `upperStr` gespeichert. Dann wird der Wert von `upperStr` in der Konsole ausgegeben.

Eine weitere Möglichkeit ist die Verwendung von Unicode-Strings und der `strings.ToUpperSpecial`-Funktion, um die Großschreibung zu erreichen.

```Go
package main

import (
    "fmt"
    "unicode"
    "unicode/utf8"
)

func main() {
    str := "äpfel"
    stru := strings.ToUpperSpecial(unicode.TurkishCase, str)
    fmt.Println(stru) // Ausgabe: ÄPFEL
}
```

Hier wird die Funktion `ToUpperSpecial` mit dem ersten Parameter `unicode.TurkishCase` aufgerufen, was bedeutet, dass die Großschreibung gemäß den Regeln der türkischen Sprache durchgeführt wird.

# Tiefergehende Informationen

Bei der Verwendung von Unicode-Strings gibt es einige wichtige Dinge zu beachten, die Auswirkungen auf die Großschreibung haben können. Zum Beispiel gibt es verschiedene Arten von Sonderzeichen, die eine Kombination aus mehreren Unicode-Codepunkten sind. In solchen Fällen kann es sein, dass die Funktion `ToUpper` oder `ToUpperSpecial` nicht wie erwartet funktionieren. Wenn Sie vollständige Kontrolle über die Großschreibung haben möchten, ist es ratsam, eine eigene Funktion zu schreiben, die die Regeln für die Sprache oder das Zeichenset berücksichtigt, mit dem Sie arbeiten.

# Siehe auch

- https://golang.org/pkg/strings/#ToUpper
- https://blog.golang.org/strings
- https://yourbasic.org/golang/uppercase-lowercase-strings/