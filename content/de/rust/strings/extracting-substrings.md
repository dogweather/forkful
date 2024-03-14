---
date: 2024-01-20 17:46:25.015258-07:00
description: "Das Extrahieren von Teilzeichenketten ist der Prozess, spezifische Teile\
  \ aus einem String herauszuschneiden. Programmierer nutzen dies f\xFCr Aufgaben\
  \ wie\u2026"
lastmod: '2024-03-13T22:44:53.661718-06:00'
model: gpt-4-1106-preview
summary: "Das Extrahieren von Teilzeichenketten ist der Prozess, spezifische Teile\
  \ aus einem String herauszuschneiden. Programmierer nutzen dies f\xFCr Aufgaben\
  \ wie\u2026"
title: Teilstrings extrahieren
---

{{< edit_this_page >}}

## Was & Warum?
Das Extrahieren von Teilzeichenketten ist der Prozess, spezifische Teile aus einem String herauszuschneiden. Programmierer nutzen dies für Aufgaben wie Datenanalyse, Formatierung oder wenn nur ein Teil des Strings weiterverarbeitet werden soll.

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
