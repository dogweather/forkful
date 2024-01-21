---
title:                "Einen neuen Projekt starten"
date:                  2024-01-20T18:04:32.148669-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen neuen Projekt starten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein neues Projekt anzufangen bedeutet, von Grund auf mit einer frischen Codebasis zu beginnen. Programmierer starten neue Projekte, um Ideen umzusetzen, Probleme zu lösen oder um mit neuen Technologien zu experimentieren.

## Anleitung:
Um ein neues Rust-Projekt zu starten, brauchst du `cargo`, den Rust-Paketmanager. Hier ist eine einfache Anleitung, um loszulegen.

```Rust
// Installiere Rust und Cargo (https://www.rust-lang.org/tools/install)

// Erstelle ein neues Projekt
cargo new mein_project

// Wechsle in das Verzeichnis des neuen Projekts
cd mein_project

// Baue das Projekt
cargo build

// Führe das Projekt aus
cargo run
```
Ausgabe bei Projektstart könnte so aussehen:
```
   Compiling mein_project v0.1.0 (/path/to/mein_project)
    Finished dev [unoptimized + debuginfo] target(s) in 0.0 secs
     Running `target/debug/mein_project`
Hello, world!
```

## Tiefergehende Informationen:
`cargo new` generiert eine Projektstruktur mit `Cargo.toml` und `main.rs` unter einem src-Verzeichnis. Historisch gesehen entstand Cargo aus dem Bedürfnis, Rust-Projekte zu verwalten und Abhängigkeiten zu handhaben, ähnlich wie Bundler in Ruby oder npm in JavaScript. Als Alternativen hätten wir Makefiles oder Skripte verwenden können, aber Cargo automatisiert diesen Prozess und bietet Reproduzierbarkeit und Konsistenz. Cargo kümmert sich um das Bauen und Testen des Codes, das Verwalten von Abhängigkeiten und das Bauen von Dokumentationen sowie das Publizieren deines Codes auf crates.io.

## Siehe auch:
- Die offizielle Cargo-Dokumentation: https://doc.rust-lang.org/cargo/
- Das Rust-Buch, um tiefer in die Sprache einzutauchen: https://doc.rust-lang.org/book/
- Ein Tutorial zum Erstellen eines realen Projekts: https://www.rust-lang.org/learn/get-started