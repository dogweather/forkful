---
title:    "Rust: Die Länge eines Strings finden."
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist ein grundlegender Schritt beim Programmieren. Mit Rust können Sie dies auf effiziente und elegante Weise tun. Lesen Sie weiter, um herauszufinden, wie!

## How To

Um die Länge eines Strings in Rust zu finden, gibt es verschiedene Möglichkeiten. Hier sind zwei Beispiele mit entsprechenden Output:

```Rust
let string = String::from("Hallo Welt");

// Verwenden Sie die Funktion .len() für die Anzahl der Bytes
println!("Die Länge des Strings in Bytes ist: {}", string.len());
// Ausgabe: Die Länge des Strings in Bytes ist: 11

// Verwenden Sie die Funktion .chars() für die Anzahl der Zeichen
println!("Die Länge des Strings in Zeichen ist: {}", string.chars().count());
// Ausgabe: Die Länge des Strings in Zeichen ist: 10
```

## Deep Dive

Es ist wichtig zu wissen, dass Strings in Rust UTF-8-encodierte Sequenzen von Bytes sind. Das bedeutet, dass die Länge eines Strings in der Anzahl der Bytes gemessen wird, nicht der Zeichen. Wenn Sie die Anzahl der Zeichen benötigen, können Sie die Funktion .chars() verwenden, wie im obigen Beispiel gezeigt.

Es gibt auch andere Funktionen wie .len_utf8() und .len_utf16(), die Ihnen helfen können, die Länge eines Strings in Bezug auf UTF-8 oder UTF-16 zu finden.

## Siehe Auch

- [Rust Dokumentation zu Strings](https://doc.rust-lang.org/std/string/)
- [Offizielle Rust Webseite](https://www.rust-lang.org/)
- [Rust Book](https://doc.rust-lang.org/stable/book/)