---
date: 2024-01-20 17:48:15.593816-07:00
description: "In Rust, die L\xE4nge eines Strings zu ermitteln bedeutet, herauszufinden,\
  \ wie viele Zeichen der String enth\xE4lt. Programmierer m\xFCssen das oft wissen,\
  \ um\u2026"
lastmod: '2024-02-25T18:49:50.732067-07:00'
model: gpt-4-1106-preview
summary: "In Rust, die L\xE4nge eines Strings zu ermitteln bedeutet, herauszufinden,\
  \ wie viele Zeichen der String enth\xE4lt. Programmierer m\xFCssen das oft wissen,\
  \ um\u2026"
title: "Ermittlung der Zeichenkettenl\xE4nge"
---

{{< edit_this_page >}}

## Was & Warum?

In Rust, die Länge eines Strings zu ermitteln bedeutet, herauszufinden, wie viele Zeichen der String enthält. Programmierer müssen das oft wissen, um Operationen wie Slicing, Iteration oder Validation durchzuführen.

## How to:

```Rust
fn main() {
    let gruss = String::from("Hallo Welt");
    let laenge = gruss.chars().count(); // Zeichen zählen
    println!("Die Länge des Strings ist: {}", laenge);
}
```

Output:
```
Die Länge des Strings ist: 10
```

Achtung: `.len()` gibt die Anzahl der Bytes zurück, nicht immer die Anzahl der Zeichen.

## Deep Dive

Die Länge eines Strings in Rust zu bestimmen, war nicht immer so direkt wie heute. Früher war es komplizierter, zuverlässig die Zeichenzahl zu bekommen, besonders mit Unicode-Zeichen. Rust schützt vor vielen Stolpersteinen, die in anderen Sprachen üblich sind.

Alternativen:
- `.len()` gibt zurück, wie viel Speicherplatz der String in Bytes belegt. Das passt, wenn nur ASCII-Zeichen verwendet werden.
- `.chars().count()` zählt die `char` Zeichen, was bei Unicode die korrekte Zeichenanzahl liefert.

Implementierungsdetails:
- Rust verarbeitet intern Strings als UTF-8, dessen Zeichen variable Länge haben. Daher ist `.chars().count()` langsamer als `.len()`, aber genauer für internationale Texte.
- Beim Umgang mit Grapheme Clustern, wie Emojis, wird's komplexer. Hier wird die `unicode-segmentation` Crate gebraucht, um genaue Ergebnisse zu erzielen.

## See Also

- Rust-Dokumentation zu Strings: https://doc.rust-lang.org/std/string/struct.String.html
- Unicode-Segmentierung in Rust: https://github.com/unicode-rs/unicode-segmentation
- Rust-String-Methoden: https://doc.rust-lang.org/std/primitive.str.html
