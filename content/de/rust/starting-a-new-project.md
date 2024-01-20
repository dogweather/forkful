---
title:                "Ein neues Projekt starten"
html_title:           "C#: Ein neues Projekt starten"
simple_title:         "Ein neues Projekt starten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Rust: Ein neues Projekt starten

## Was & Warum?

Ein neues Projekt zu starten bedeutet, von grund auf eine Software zu entwickeln. Programmierer machen das, um Lösungen auf maßgeschneiderte Weise anzubieten.

## Wie geht das:

Erstellen Sie mit Cargo ein neues Projekt:

```Rust
$ cargo new mein_projekt
```

Navigieren Sie zu diesem:

```Rust
$ cd mein_projekt
```

Jetzt finden Sie eine Cargo.toml-Datei, die Ihre Dependencies und andere Einstellungen enthält, und eine src/main.rs-Datei, die das Hauptprogramm enthält:

```Rust
$ cat Cargo.toml

[package]
name = "mein_projekt"
version = "0.1.0"
authors = ["Your Name <you@example.com>"]
edition = "2018"

# See more keys and their definitions at https://doc.rust-lang.org/cargo/reference/manifest.html

[dependencies]

.

$ cat src/main.rs

fn main() {
    println!("Hello, world!");
}
```

## Tiefgehende Infos:

Historisch gesehen ermöglichten Programmiersprachen wie C und C++ die Erstellung solcher Projekte, aber sie brachten viele Probleme mit sich, insbesondere im Bereich Speichersicherheit. Rust wurde entwickelt, um diese Probleme zu lösen und gleichzeitig eine hochperformante Systemsprache zu bieten.

Alternativen zu Rust könnten Go, Python und JavaScript sein, aber sie erreichen nicht das gleiche Maß an Speichersicherheit und Leistung, die ein Rust-Projekt bieten kann.

In der Praxis erfordert der Start eines neuen Projekts in Rust nicht viel mehr als ein paar Befehle. Mit Hilfe von Cargo, dem Paketmanager und dem Buildsystem von Rust, lässt sich ein neues Projekt schnell und einfach einrichten.

## Weitere Informationen:

Für weitere Informationen besuchen Sie bitte die folgenden Links:

- [Offizielle Rust-Dokumentation](https://doc.rust-lang.org/book/)
- [Rust auf GitHub](https://github.com/rust-lang/rust)
- [Cargo-Dokumentation](https://doc.rust-lang.org/cargo/)