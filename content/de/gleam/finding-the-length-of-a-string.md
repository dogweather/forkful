---
title:    "Gleam: Die Länge eines Strings finden"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine grundlegende Aufgabe in der Programmierung und kann sehr nützlich sein, um verschiedene Operationen auf einem String auszuführen. Zum Beispiel kann die Länge eines Strings verwendet werden, um zu überprüfen, ob er leer ist oder um ihn in Substrings aufzuteilen.

## Wie

Die Länge eines Strings kann in Gleam mithilfe der Funktion `string.length()` gefunden werden. Diese Funktion akzeptiert einen String als Argument und gibt die Anzahl der Zeichen in dem String zurück.

```Gleam
let str = "Hallo, Welt!"
let len = string.length(str)

// Output: 12
```

Es ist auch möglich, die Länge eines Strings in Gleam mithilfe von Mustern zu finden. Dabei wird der String in Substrings aufgeteilt und die Länge der Substrings wird gezählt.

```Gleam
let str = "Hallo, Welt!"

case string.split(str, "") {
    [len1, len2] -> len1 + len2 + 1
}

// Output: 12
```

## Deep Dive

Beim Finden der Länge eines Strings ist es wichtig zu beachten, dass Leerzeichen und Sonderzeichen ebenfalls als Zeichen gezählt werden. Dies kann zu unerwarteten Ergebnissen führen, wenn man z.B. versucht, die Länge eines Strings zu finden, der HTML-Tags oder Escape-Zeichen enthält.

Um die Länge eines Strings zu finden, der keine Leerzeichen oder Sonderzeichen enthält, kann die Gleam-Funktion `string.chars()` verwendet werden. Diese Funktion gibt eine Liste der Zeichen in dem String zurück, die dann gezählt werden können.

```Gleam
let str = "Hallo, Welt!"
let len = str |> string.chars |> list.length

// Output: 11
```

## Siehe auch

- Gleam Dokumentation über das `string` Modul: https://gleam.run/documentation/stdlib/string
- Tutorial zum Arbeiten mit Strings in Gleam: https://gleam.run/articles/strings