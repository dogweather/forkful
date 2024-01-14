---
title:                "Gleam: Die Länge eines Strings ermitteln"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Warum

Das Finden der Länge eines Strings ist ein wichtiger Teil der Programmierung, da es uns ermöglicht, die Größe von Texten oder Zeichenketten zu bestimmen. Dies ist besonders nützlich, wenn wir mit Benutzereingaben oder Daten aus verschiedenen Quellen arbeiten.

##Wie

Um die Länge eines Strings in Gleam zu finden, verwenden wir die Funktion `String.length`. Diese Funktion nimmt einen String als Argument und gibt die Anzahl der darin enthaltenen Zeichen zurück.

```Gleam
let text = "Hallo Welt"
let length = String.length(text)
```
In diesem Beispiel haben wir den String "Hallo Welt" der Variablen `text` zugewiesen und die Länge des Strings mit `String.length` in der Variable `length` gespeichert. Die Ausgabe wird die Zahl 11 sein, da der String 11 Zeichen enthält.

```Gleam
let empty = ""
let empty_length = String.length(empty)
```
In diesem Beispiel haben wir einen leeren String der Variablen `empty` zugewiesen und die Länge des Strings mit `String.length` in der Variablen `empty_length` gespeichert. Die Ausgabe wird 0 sein, da der leere String keine Zeichen enthält.

##Tiefer Einblick

Es ist wichtig zu beachten, dass Gleam die Länge eines Strings in Bytes zählt, nicht in Zeichen. Dies kann manchmal zu unerwarteten Ergebnissen führen, insbesondere wenn wir mit Unicode-Zeichen arbeiten, die aus mehreren Bytes bestehen können. In solchen Fällen können wir die Funktion `String.utf8_length` verwenden, um die Länge des Strings in Zeichen zu erhalten.

```Gleam
let unicode_text = "✨Gleam✨"
let length = String.length(unicode_text)
let utf8_length = String.utf8_length(unicode_text)
```
In diesem Beispiel enthält der String "✨Gleam✨" sowohl das Unicode-Zeichen "✨" als auch das "einfache" Zeichen "Gleam". Die Ausgabe von `String.length` wird 12 sein, während die Ausgabe von `String.utf8_length` 8 sein wird, da das Unicode-Zeichen aus 3 Bytes besteht.

##Siehe auch

- Offizielle Gleam-Dokumentation zu `String.length`: https://gleam.run/docs/standard-library/string/#length
- Offizielle Gleam-Dokumentation zu `String.utf8_length`: https://gleam.run/docs/standard-library/string/#utf8_length
- Tutorial zur Arbeit mit Strings in Gleam: https://gleam.run/tutorials/strings/