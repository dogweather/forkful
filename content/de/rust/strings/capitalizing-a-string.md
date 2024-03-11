---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:29.754150-07:00
description: "Das Gro\xDFschreiben eines Strings in Rust involviert das Modifizieren\
  \ des Strings, sodass sein erstes Zeichen ein Gro\xDFbuchstabe ist, falls es sich\
  \ um einen\u2026"
lastmod: '2024-03-11T00:14:27.547117-06:00'
model: gpt-4-0125-preview
summary: "Das Gro\xDFschreiben eines Strings in Rust involviert das Modifizieren des\
  \ Strings, sodass sein erstes Zeichen ein Gro\xDFbuchstabe ist, falls es sich um\
  \ einen\u2026"
title: "Einen String gro\xDFschreiben"
---

{{< edit_this_page >}}

## Was & Warum?

Das Großschreiben eines Strings in Rust involviert das Modifizieren des Strings, sodass sein erstes Zeichen ein Großbuchstabe ist, falls es sich um einen Buchstaben handelt, während der Rest des Strings unverändert bleibt. Programmierer führen diese Operation oft für Formatierungszwecke durch, wie zum Beispiel die Vorbereitung von Wörtern für Titel oder um Konsistenz in Benutzereingaben zu gewährleisten.

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
