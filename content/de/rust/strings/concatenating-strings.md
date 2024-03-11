---
date: 2024-01-20 17:35:46.730033-07:00
description: "Stringverkettung bedeutet, zwei oder mehr Strings zu einem einzigen\
  \ zusammenzuf\xFCgen. Programmierer machen das, um dynamische Texte zu erzeugen\
  \ oder Daten\u2026"
lastmod: '2024-03-11T00:14:27.555904-06:00'
model: gpt-4-1106-preview
summary: "Stringverkettung bedeutet, zwei oder mehr Strings zu einem einzigen zusammenzuf\xFC\
  gen. Programmierer machen das, um dynamische Texte zu erzeugen oder Daten\u2026"
title: "Zeichenketten verkn\xFCpfen"
---

{{< edit_this_page >}}

## What & Why?
Stringverkettung bedeutet, zwei oder mehr Strings zu einem einzigen zusammenzufügen. Programmierer machen das, um dynamische Texte zu erzeugen oder Daten aus verschiedenen Quellen zu kombinieren.

## How to:
Verkettung mit `+`:

```Rust
fn main() {
    let gruss = "Hallo".to_string();
    let welt = "Welt!";
    let kompletter_gruss = gruss + " " + welt;
    println!("{}", kompletter_gruss);
}
```
Ausgabe:
```
Hallo Welt!
```

Mit `format!` Makro:

```Rust
fn main() {
    let tier = "Fuchs";
    let laut = "schnell";
    let satz = format!("Der {} ist {}", tier, laut);
    println!("{}", satz);
}
```
Ausgabe:
```
Der Fuchs ist schnell
```

## Deep Dive
Stringverkettung gibt's, seit es Programmiersprachen gibt. In Rust gibt es mehrere Wege, Strings zu verketten, weil es verschiedene Anwendungsfälle gibt. `+` nimmt den Besitz des ersten Strings und hängt die weiteren Strings hinten an. Es ist schnell, aber wenig flexibel. `format!`, ein Makro, ist flexibler und erlaubt es, komplexe Textmuster zu erstellen, kann aber langsamer sein, insbesondere bei großen Datenmengen.

Interner Ablauf: `+` verwendet unter der Haube die `add` Methode, die wiederum `push_str` nutzt, um den String zu erweitern. Rust benötigt für den ersten String Besitzrechte, da es dessen interne Darstellung direkt verändert. Die Flexibilität von `format!` resultiert aus der Tatsache, dass es unter der Haube mit einem `Formatter` arbeitet, der temporäre Puffer für die Textdaten verwendet.

## See Also
- Die offizielle Rust-Dokumentation für `String`: https://doc.rust-lang.org/std/string/struct.String.html
- Rust Book über Stringverkettung: https://doc.rust-lang.org/book/ch08-02-strings.html#concatenation-with-the--operator-or-the-format-macro
- Rust by Example für verschiedene Stringoperationen: https://doc.rust-lang.org/rust-by-example/std/str.html
