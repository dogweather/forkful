---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
date:                  2024-01-20T17:43:04.202690-07:00
model:                 gpt-4-1106-preview
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Rust-Programmierung: Löschung von Zeichen nach Muster

## Was & Warum?
Zeichen, die einem Muster entsprechen, zu löschen, bedeutet, aus einem Text gezielt bestimmte Elemente zu entfernen. Programmierer nutzen diese Operation, um unnötige oder unerwünschte Daten zu säubern oder Formatierungen zu standardisieren.

## So geht's:
```Rust
fn main() {
    let text = "F0o B4r ist 100% toll!";
    let pattern = |c: char| c.is_digit(10) || !c.is_alphanumeric();
    let result: String = text.chars().filter(|&c| !pattern(c)).collect();
    
    println!("Vorher: {}", text);
    println!("Nachher: {}", result);
}
```

Ausgabe:
```
Vorher: F0o B4r ist 100% toll!
Nachher: F0o Br ist toll
```

## Tiefere Einblicke:
Früher wurden solche Muster mit regulären Ausdrücken oder eigenen Algorithmen gelöscht. In Rust gibt es effiziente Methoden wie die `filter`-Methode, die zusammen mit Lambda-Funktionen oder Closures flexibel einsetzbar ist. Alternativen dazu sind z.B. direkte Iterationen über Zeichen und manuelles Zusammensetzen von Strings oder die Nutzung von Textverarbeitungsbibliotheken wie `regex`.

Bei der Leistung spielt auch der Charakter der Muster eine Rolle. Einfache Operationen wie das Entfernen von Ziffern oder Satzzeichen sind mit Rusts Standard-Funktionen schnell erledigt. Komplexere Muster benötigen möglicherweise den Einsatz von `regex` für effektive Verarbeitung.

## Weiterführende Informationen:
- Rust Standard Library: [https://doc.rust-lang.org/std/]
- Rust Regex Crate Dokumentation: [https://docs.rs/regex/]
- Rust by Example - Textverarbeitung: [https://doc.rust-lang.org/rust-by-example/std/str.html]
