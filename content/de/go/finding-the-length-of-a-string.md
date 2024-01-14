---
title:    "Go: Die Länge eines Strings finden"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine grundlegende, aber dennoch wichtige Aufgabe in der Programmierung. Oft müssen wir die Länge eines Strings für verschiedene Operationen wie das Zählen von Zeichen oder das Überprüfen der Eingabevalidierung ermitteln. In diesem Blogbeitrag werden wir uns ansehen, wie wir die Länge eines Strings in der Go-Programmierung finden können.

## Wie geht man vor?

Um die Länge eines Strings in Go zu finden, können wir die `len()` Funktion verwenden. Diese Funktion gibt uns die Anzahl der Bytes zurück, die ein String belegt. Schauen wir uns ein paar Beispiele an:

```Go
package main

import "fmt"

func main() {
    // Beispiel 1
    str1 := "Hello world"
    fmt.Println(len(str1)) // Output: 11 (11 Bytes)

    // Beispiel 2
    str2 := "Gophers"
    fmt.Println(len(str2)) // Output: 7 (7 Bytes)
}
```

In unserem ersten Beispiel haben wir einen String mit dem Wert "Hello world" erstellt. Die `len()` Funktion gibt uns den Wert 11 zurück, da der String aus 11 Bytes besteht. Das Leerzeichen zwischen "Hello" und "world" wird als ein Byte gezählt.

Im zweiten Beispiel haben wir einen String mit dem Wert "Gophers" erstellt. Hier gibt uns die `len()` Funktion den Wert 7 zurück, da dieser String ebenfalls aus 7 Bytes besteht.

Es ist wichtig zu beachten, dass die `len()` Funktion die Anzahl der Bytes und nicht der Zeichen im String zurückgibt. In Go werden Strings als UTF-8 codiert, wodurch manche Zeichen aus mehr als einem Byte bestehen.

## Tiefergehende Betrachtung

Um die Länge eines Strings zu verstehen, müssen wir etwas tiefer in die Details der Go-Programmierung eintauchen. Wie bereits erwähnt, werden Strings in Go als UTF-8 codiert. Dies bedeutet, dass ein einzelnes Zeichen mehrere Bytes umfassen kann, je nachdem, welches Zeichen es ist.

Die `len()` Funktion gibt uns also die Anzahl der Bytes zurück, die ein String belegt, unabhängig davon, wie viele Zeichen er enthält. Um die Anzahl der Zeichen in einem String zu ermitteln, können wir die `utf8.RuneCountInString()` Funktion verwenden. Diese Funktion gibt uns die Anzahl der Zeichen im String zurück, anstatt die Anzahl der Bytes.

Schauen wir uns ein Beispiel an:

```Go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    // Beispiel 1
    str1 := "Hello world"
    fmt.Println(len(str1)) // Output: 11 (11 Bytes)
    fmt.Println(utf8.RuneCountInString(str1)) // Output: 11 (11 Zeichen)

    // Beispiel 2
    str2 := "Golang 🚀"
    fmt.Println(len(str2)) // Output: 11 (11 Bytes)
    fmt.Println(utf8.RuneCountInString(str2)) // Output: 8 (8 Zeichen)
}
```

In unserem ersten Beispiel sehen wir, dass `len()` und `utf8.RuneCountInString()` beide den Wert 11 zurückgeben, da der String aus 11 Zeichen besteht.

Im zweiten Beispiel sehen wir jedoch einen Unterschied. Die `len()` Funktion gibt immer noch den Wert 11 zurück, da der String 11 Bytes belegt, aber die `utf8.RuneCountInString()` Funktion gibt den Wert 8 zurück, da der String tatsächlich nur 8 Zeichen enthält.

Dies liegt daran, dass das Emoji "🚀" aus mehreren Bytes besteht und somit als ein Zeichen gezählt wird, während die `len()` Funktion die Anzahl der Bytes zurückgibt, die für das Emoji benötigt werden.

## Siehe Auch

- [Go-Dokumentation: Strings](https://golang.org/ref/spec#String_types)
- [Tutorial: Working with UTF-8 encoded strings in Go](https://www.calhoun.io/working-with-utf-8-encoded-strings-in-go/)
- [Tutorial: Understanding string indexing in Go](https://go101.org/article/string-indexing.html)