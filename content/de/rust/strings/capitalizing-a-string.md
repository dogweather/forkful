---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:29.754150-07:00
description: "Wie: Um einen String in Rust zu gro\xDFzuschreiben, haben Sie zwei Hauptwege:\
  \ die Nutzung von Standardbibliotheksfunktionalit\xE4ten oder die Verwendung von\u2026"
lastmod: '2024-03-13T22:44:53.655990-06:00'
model: gpt-4-0125-preview
summary: "Um einen String in Rust zu gro\xDFzuschreiben, haben Sie zwei Hauptwege."
title: "Einen String gro\xDFschreiben"
weight: 2
---

## Wie:
Um einen String in Rust zu großzuschreiben, haben Sie zwei Hauptwege: die Nutzung von Standardbibliotheksfunktionalitäten oder die Verwendung von Drittanbieter-Crates für komplexere oder spezifischere Bedürfnisse. Hier ist, wie Sie beides machen können.

### Verwendung der Standardbibliothek von Rust
Die Standardbibliothek von Rust bietet keine direkte Methode, um Strings großzuschreiben, aber Sie können dies erreichen, indem Sie die Zeichen des Strings manipulieren.

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hallo";
    println!("{}", capitalize_first(my_string)); // Ausgabe: Hallo
}
```

### Verwendung des `heck` Crates
Für einen geradlinigeren Ansatz, besonders wenn Sie im Kontext einer größeren Textverarbeitung arbeiten, bevorzugen Sie möglicherweise die Verwendung von Drittanbieter-Bibliotheken wie `heck`. Das `heck` Crate bietet verschiedene Funktionalitäten zur Fallumwandlung, einschließlich einer einfachen Methode, um Strings großzuschreiben.

Zuerst fügen Sie `heck` zu Ihrem `Cargo.toml` hinzu:

```toml
[dependencies]
heck = "0.4.0"
```

Dann verwenden Sie es, um Ihren String großzuschreiben:

```rust
extern crate heck; // Nicht benötigt in der Rust 2018 Edition oder später
use heck::TitleCase;

fn main() {
    let my_string = "hallo welt";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // Ausgabe: Hallo Welt
}
```

Hinweis: Die Methode `to_title_case`, die von `heck` bereitgestellt wird, schreibt jedes Wort im String groß, was mehr sein könnte, als Sie suchen, wenn Sie nur den ersten Buchstaben des Strings großgeschrieben haben wollen. Passen Sie Ihre Nutzung entsprechend Ihren spezifischen Bedürfnissen an.
