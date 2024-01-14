---
title:                "Rust: Die Länge eines Strings finden."
simple_title:         "Die Länge eines Strings finden."
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum 

Das Finden der Länge eines Strings mag auf den ersten Blick wie eine einfache Aufgabe erscheinen, aber es ist ein wichtiger Teil des Programmierens. Indem man die Länge eines Strings kennen und verwenden kann, kann man komplexe Algorithmen und Datenstrukturen bauen.

## Wie geht man vor

Um die Länge eines Strings in Rust zu finden, gibt es eine eingebaute Funktion namens `len()`. Diese Funktion gibt die Anzahl der Unicode-Scalar-Werte eines Strings zurück, was der Anzahl der Buchstaben im String entspricht. Schauen wir uns ein einfaches Beispiel an:

```Rust
fn main() {
    let my_string = String::from("Hallo");
    println!("Die Länge meines Strings ist {}", my_string.len());
}
```

Die Ausgabe dieses Codes wird `Die Länge meines Strings ist 5` sein. Wie man sehen kann, gibt `len()` die Anzahl der Buchstaben im String zurück, unabhängig von der tatsächlichen Anzahl von Bytes, die der String belegt.

Ein weiteres wichtiges Konzept beim Finden der Länge eines Strings ist, dass Unicode-Scalar-Werte verschiedene Längen haben können. Zum Beispiel hat das deutsche Umlaut `ä` einen Unicode-Scalar-Wert von 228, während das englische `a` einen Unicode-Scalar-Wert von 97 hat. Dies bedeutet, dass die Länge von Strings mit Umlauten möglicherweise nicht der tatsächlichen Anzahl von Zeichen entspricht. Hier ist ein Beispiel, das dies veranschaulicht:

```Rust
fn main() {
    let my_string = String::from("Hällo");
    println!("Die Länge meines Strings ist {}", my_string.len());
}
```

Die Ausgabe dieses Codes wird `Die Länge meines Strings ist 5` sein, obwohl es nur 4 Zeichen im String gibt. Dies liegt daran, dass der Umlaut `ä` einen Unicode-Scalar-Wert von 228 hat, was von `len()` mitgezählt wird.

## Tiefergehend

Ein wichtiger Punkt beim Finden der Länge eines Strings in Rust ist das Verständnis der Unicode-Darstellung. Rust verwendet UTF-8-Codierung, was bedeutet, dass Sonderzeichen und Umlaute mehr als ein Byte belegen können. Bei der Verwendung von `len()` wird jedoch die Anzahl der Unicode-Scalar-Werte zurückgegeben, unabhängig von der tatsächlichen Anzahl von Bytes. Dies bedeutet, dass die Länge eines Strings in Rust immer die Anzahl der Zeichen zurückgibt, während die tatsächliche Anzahl von Bytes variiert.

## Siehe auch

- [Offizielle Dokumentation von Rust zur Funktion len()](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [Tutorial von Rust zu Unicode und UTF-8](https://www.rust-lang.org/learn/strings#unicode-and-utf-8)