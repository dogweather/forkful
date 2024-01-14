---
title:                "Go: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge einer Zeichenkette ist eine grundlegende Aufgabe in der Programmierung und hilft dabei, Daten effizient zu verarbeiten.

## Wie geht das

Um die Länge einer Zeichenkette in Go zu finden, können wir die Funktion "len" verwenden. Sie gibt die Anzahl der Zeichen in der Zeichenkette zurück.

```Go
s := "Hallo Welt"
fmt.Println(len(s))
```

Output:
`11`

Ein weiterer Weg ist die Verwendung der Funktion "RuneCountInString", die auch Unicode-Zeichen zählt.

```Go
s := "Hello, 世界"
fmt.Println(len(s))
fmt.Println(utf8.RuneCountInString(s))
```

Output:
```Go
13
9
```

## Tiefer Einblick

In Go werden Zeichenketten als Byte-Slice von Zeichen dargestellt. Dies bedeutet, dass jedes Zeichen in der Zeichenkette einen bestimmten Speicherplatz benötigt und die Gesamtlänge der Zeichenkette auch vom verwendeten Zeichensatz abhängig ist.

Eine wichtige Sache, die man beachten muss, ist, dass die Länge einer Zeichenkette immer die Anzahl der Zeichen und nicht die Anzahl der Bytes ist. Dies kann zu Problemen führen, wenn man mit mehreren Zeichensätzen arbeitet, da die Anzahl der Bytes möglicherweise nicht mit der Anzahl der Zeichen übereinstimmt.

## Siehe auch

- [Go Standardbibliothek: Strings](https://golang.org/pkg/strings/)
- [How to Find the Length of a String in Go](https://www.digitalocean.com/community/tutorials/how-to-find-the-length-of-a-string-in-go)
- [How to Work with Strings in Go](https://www.digitalocean.com/community/tutorials/how-to-work-with-strings-in-go)