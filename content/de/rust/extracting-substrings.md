---
title:                "Unterstrings extrahieren"
html_title:           "Rust: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

In einer Welt, in der Textverarbeitung und Manipulation immer wichtiger werden, ist die Fähigkeit, Teilstrings zu extrahieren, ein entscheidender Faktor für die Effizienz beim Programmieren. Mit Rust können wir dies auf einfache und effektive Weise tun, indem wir die integrierten String-Funktionen nutzen.

## So geht's

Um Teilstrings in Rust zu extrahieren, verwenden wir die Funktion `slice()` zusammen mit dem `..` Operator, der einen Teilbereich aus einem String auswählt. Wir erstellen einen neuen String, der den extrahierten Teil enthält und geben ihn aus.

```Rust
let string = String::from("Hallo Welt!");
let substring = &string[6..]; // Ausgabe: "Welt!"
```

Weitere nützliche Funktionen zum Extrahieren von Teilstrings sind `find()` und `split()`. Mit `find()` können wir nach bestimmten Zeichen oder Mustern suchen und den Teilstring bis zu diesem Punkt extrahieren. `split()` hingegen teilt den String in Teile auf und gibt diese als Vektoren aus.

```Rust
let string = String::from("Rust ist großartig!");
let index = string.find("ist").unwrap(); // Ausgabe: 5
let substring = &string[..index]; // Ausgabe: "Rust"

let string = String::from("1,2,3");
let array: Vec<&str> = string.split(",").collect(); // Ausgabe: ["1", "2", "3"]
```

## Tiefer graben

Hinter den Kulissen speichert Rust Strings als Byte-Arrays und die Indexierung ist daher in Byte-Schritten. Dies bedeutet, dass der `slice()` Operator untersucht, wie viele Bytes der Teilstring enthält, anstatt einfach die Anzahl der Zeichen zu zählen. Dies kann zu unerwarteten Ergebnissen führen, wenn der String non-ASCII-Zeichen enthält, da diese mehr als ein Byte belegen.

Es ist auch wichtig zu beachten, dass `slice()` eine Referenz auf den Teilstring erstellt, anstatt eine neue String-Instanz zu erstellen. Dies reduziert die Anzahl der Speicherzuweisungen und verbessert die Leistung, ist aber auch zu beachten, wenn der ursprüngliche String weiterhin verwendet werden soll.

## Siehe auch

Weitere Informationen über das Arbeiten mit Strings in Rust finden Sie in der offiziellen Dokumentation unter:

- https://doc.rust-lang.org/book/ch08-02-strings.html
- https://doc.rust-lang.org/stable/std/string/struct.String.html