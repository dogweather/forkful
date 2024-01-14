---
title:    "Go: Die Länge eines Strings ermitteln"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist ein grundlegender Prozess in der Programmierung. Es ermöglicht uns, zu bestimmen, wie viele Zeichen ein String enthält, was in vielen Situationen hilfreich sein kann, wie zum Beispiel bei der Validierung von Benutzereingaben oder der Manipulation von Text.

## Wie geht man vor

Es gibt mehrere Möglichkeiten, die Länge eines Strings in Go zu finden. Eine Möglichkeit ist die Verwendung der built-in Funktion `len()`, die die Anzahl der Zeichen in einem String zurückgibt:

```Go
str := "Hallo Welt"
length := len(str)
fmt.Println(length) // Output: 10
```

Eine weitere Möglichkeit ist die Verwendung des Packages "strings" und der Funktion `Count()`, um die Anzahl eines bestimmten Zeichens in einem String zu zählen:

```Go
import "strings"

str := "Hello World"
count := strings.Count(str, "l")
fmt.Println(count) // Output: 3
```

## Tieferer Einblick

Die `len()` Funktion zählt nicht nur die Buchstaben in einem String, sondern auch andere Zeichen wie Leerzeichen, Interpunktionszeichen und Sonderzeichen. Dies kann zu unerwarteten Ergebnissen führen, insbesondere wenn man mit Strings aus verschiedenen Sprachen arbeitet, die verschiedene Zeichensätze verwenden.

Um dies zu vermeiden, kann die `Count()` Funktion aus dem "strings" Package verwendet werden, indem sie nach einem spezifischen Zeichen sucht und diese als Kriterium für die Zählung verwendet.

Wenn man die Länge eines Unicode-Strings finden möchte, kann man die Funktion `RuneCountInString()` aus dem "unicode/utf8" Package verwenden. Diese zählt die Anzahl der Unicode-Codepoints im String, unabhängig von deren Darstellung oder Länge.

## Siehe auch

- [The Go Programming Language](https://golang.org/)
- [Go Strings Package](https://pkg.go.dev/strings)
- [Go Unicode Package](https://pkg.go.dev/unicode/utf8)