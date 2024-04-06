---
date: 2024-01-20 17:46:25.015258-07:00
description: "How to: In der Rust-Programmierung ist String-Handling besonders wichtig\
  \ und manchmal knifflig, da Rust Speichersicherheit garantiert. Historisch gesehen,\u2026"
lastmod: '2024-04-05T21:53:55.541012-06:00'
model: gpt-4-1106-preview
summary: In der Rust-Programmierung ist String-Handling besonders wichtig und manchmal
  knifflig, da Rust Speichersicherheit garantiert.
title: Teilstrings extrahieren
weight: 6
---

## How to:
```Rust
fn main() {
    let text = "Hallo, Rust-Entwickler!";
    let start = 7;
    let end = 11;
    let substring = &text[start..end];
    println!("Extrahierter Teilstring: '{}'", substring);
}
```
Ausgabe:
```
Extrahierter Teilstring: 'Rust'
```

## Deep Dive
In der Rust-Programmierung ist String-Handling besonders wichtig und manchmal knifflig, da Rust Speichersicherheit garantiert. Historisch gesehen, waren Operationen auf Zeichenketten in vielen Sprachen fehleranfällig. Rust vermeidet viele dieser Probleme durch seine konsequente Behandlung von Strings als Byte-Arrays.

Andere Wege, um Teilstrings zu extrahieren, beinhalten Methoden wie `.trim()`, `.split()` und `.matches()`, die verschiedene Anwendungsfälle abdecken können. Zum Beispiel ist `.trim()` gut, um whitespace zu entfernen, während `.split()` hilfreich ist, um einen String auf Basis eines Trennzeichens zu teilen.

Die Spezifikation von Start- und Endindex innerhalb des Slicing-Vorgangs `[start..end]` berücksichtigt die Bytegrenzen von UTF-8 kodierten Zeichen, was in Rust unabdingbar ist, da ein naiver Ansatz, der einfach Byte-Positionen schneidet, gültige Zeichen zerstören kann. 

Das Slicing ohne explizite Grenzen `text[..]` gibt den ganzen String zurück.

## See Also
- Die Rust-Dokumentation über Strings: https://doc.rust-lang.org/book/ch08-02-strings.html
- Rust by Example für praxisnahe Beispiele: https://doc.rust-lang.org/rust-by-example/std/str.html
- Ein Blogpost zur Effizienz von Rust Strings: https://blog.rust-lang.org/2015/01/30/Rust-Trait-Objects.html
