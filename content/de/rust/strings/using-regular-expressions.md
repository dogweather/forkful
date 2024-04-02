---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:12.602554-07:00
description: "Regul\xE4re Ausdr\xFCcke, oder Regex, erm\xF6glichen es Entwicklern,\
  \ mit fortgeschrittenen Mustervergleichstechniken in Zeichenketten zu suchen, \xDC\
  bereinstimmungen\u2026"
lastmod: '2024-03-13T22:44:53.662854-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE4re Ausdr\xFCcke, oder Regex, erm\xF6glichen es Entwicklern, mit\
  \ fortgeschrittenen Mustervergleichstechniken in Zeichenketten zu suchen, \xDCbereinstimmungen\u2026"
title: "Regul\xE4re Ausdr\xFCcke verwenden"
weight: 11
---

## Was & Warum?

Reguläre Ausdrücke, oder Regex, ermöglichen es Entwicklern, mit fortgeschrittenen Mustervergleichstechniken in Zeichenketten zu suchen, Übereinstimmungen zu finden und diese zu manipulieren. In Rust hilft die Nutzung von Regex dabei, das Parsen und den Umgang mit Textdaten effizient zu gestalten, wodurch Aufgaben wie Datenvalidierung, Suche und Texttransformationen einfacher und wartbarer werden.

## Wie geht das:

Die `regex` Bibliothek in Rust ist der Anlaufpunkt, um mit regulären Ausdrücken zu arbeiten. Um sie zu nutzen, müssen Sie sie zuerst in Ihre `Cargo.toml` hinzufügen:

```toml
[dependencies]
regex = "1"
```

Danach können Sie anfangen, Regex-Funktionalitäten in Ihrem Rust-Code zu implementieren. Hier ist, wie man einige gängige Operationen durchführt:

### Ein Muster in einer Zeichenkette finden

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let datum = "2023-04-15";

    println!("Entspricht der Text dem Datumsformat? {}", re.is_match(datum));
    // Ausgabe: Entspricht der Text dem Datumsformat? true
}
```

### Übereinstimmungen finden und darauf zugreifen

```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("Sprache: {}, Jahr: {}", &cap[1], &cap[2]);
    }
    // Ausgabe:
    // Sprache: Rust, Jahr: 2023
    // Sprache: C++, Jahr: 2022
    // Sprache: Python, Jahr: 2021
}
```

### Text ersetzen

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let replaced = re.replace_all(text, "$1 wurde im Jahr $2 aktualisiert");

    println!("Aktualisierter Text: {}", replaced);
    // Ausgabe: Aktualisierter Text: Rust wurde im Jahr 2023 aktualisiert, C++ wurde im Jahr 2022 aktualisiert, Python wurde im Jahr 2021 aktualisiert
}
```

### Text mit einem Regex teilen

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // teilt bei jedem Nicht-Wort-Zeichen
    let text = "Rust-C++-Python-Go";

    let felder: Vec<&str> = re.split(text).collect();

    for feld in felder {
        println!("Sprache: {}", feld);
    }
    // Ausgabe:
    // Sprache: Rust
    // Sprache: C++
    // Sprache: Python
    // Sprache: Go
}
```

Diese Beispiele bieten eine grundlegende Anleitung für den Einstieg in die Arbeit mit regulären Ausdrücken in Rust. Sobald Ihre Anforderungen komplexer werden, bietet die `regex`-Crate eine Fülle von Funktionen für komplexe Mustervergleiche und Textmanipulationsaufgaben.
