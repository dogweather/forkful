---
title:                "Go: Die Länge eines Strings finden"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge einer Zeichenkette ist eine grundlegende Fähigkeit in der Programmierung, die für viele Anwendungen unerlässlich ist. In diesem Blogpost werden wir uns ansehen, wie man dies in der Go Programmiersprache umsetzt.

## Wie geht das

Um die Länge einer Zeichenkette in Go zu finden, können wir die Funktion `len()` verwenden. Diese Funktion gibt die Anzahl der Zeichen in einer Zeichenkette zurück. Schauen wir uns ein Beispiel an:

```Go
package main

import "fmt"

func main() {
  text := "Hallo, Welt!"
  length := len(text)
  fmt.Println(length) // Ausgabe: 12
}
```
In diesem Beispiel haben wir eine Variable `text` erstellt, die den Wert "Hallo, Welt!" enthält. Mit der Funktion `len()` finden wir dann die Länge dieser Zeichenkette und speichern sie in der Variablen `length`. Anschließend geben wir die Länge auf der Konsole aus und erhalten die Ausgabe 12, da die Zeichenkette 12 Zeichen lang ist.

## Tiefere Einblicke

Wenn wir uns genauer ansehen, wie die `len()` Funktion in Go funktioniert, sehen wir, dass sie bei der Berechnung der Länge der Zeichenkette die Unicode-Zeichen berücksichtigt. Das bedeutet, dass auch Sonderzeichen und Emojis in die Länge mit einbezogen werden.

Außerdem ist es wichtig zu beachten, dass die Länge einer Zeichenkette auch von der verwendeten Codierung abhängig sein kann. In Go wird standardmäßig die UTF-8 Codierung verwendet, was bedeutet, dass jedes Zeichen mit einem Codepunkt von 1 bis 4 Bytes dargestellt werden kann. Dadurch kann die Länge einer Zeichenkette je nach verwendeten Zeichen variieren.

## Siehe auch

- Offizielle Go Dokumentation zur `len()` Funktion: https://golang.org/pkg/builtin/#len
- Tutorial zur Zeichenkettenverarbeitung in Go: https://www.digitalocean.com/community/tutorials/how-to-use-strings-in-go