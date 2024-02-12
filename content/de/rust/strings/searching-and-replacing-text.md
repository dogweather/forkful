---
title:                "Suchen und Ersetzen von Text"
aliases:
- /de/rust/searching-and-replacing-text.md
date:                  2024-01-20T17:58:50.479445-07:00
model:                 gpt-4-1106-preview
simple_title:         "Suchen und Ersetzen von Text"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Suchen und Ersetzen von Text ermöglicht es, spezifische Zeichenkombinationen in einem Datenstrom zu finden und durch neue Inhalte zu ersetzen. Es ist ein grundlegendes Werkzeug für Programmierer, um Daten zu manipulieren, Fehler zu korrigieren oder Batch-Operationen durchzuführen.

## How to:
Die Standardbibliothek von Rust bietet mächtige Methoden zum Textersatz. Hier ein einfaches Beispiel:

```Rust
fn main() {
    let text = "Frohe Weihnachten!";
    let updated_text = text.replace("Weihnachten", "Ostern");
    println!("{}", updated_text);
}
```

Ausgabe:

```
Frohe Ostern!
```

Für reguläre Ausdrücke benutzt man das `regex` Crates:

```Rust
use regex::Regex;

fn main() {
    let regel = Regex::new(r"Weihnachten").unwrap();
    let text = "Frohe Weihnachten!";
    let nachher = regel.replace_all(text, "Ostern");
    println!("{}", nachher);
}

```

Ausgabe:

```
Frohe Ostern!
```

## Deep Dive
Textsuche und -ersatz gibt's schon ewig. Sed, ein Stream-Editor für Unix-Systeme, nutzt seit den 1970ern reguläre Ausdrücke für solche Operationen. In Rust haben wir ähnliche Funktionen direkt in der Standardbibliothek oder durch Crates wie `regex` für komplexere Suchmuster.

Man unterscheidet zwischen simplen Textersetzungsmethoden wie `replace()` und "Regular Expressions" (Regex), die flexible Suchmuster erkennen. Regex ist mächtig, aber langsamer und schwieriger zu lesen.

Beim Arbeiten mit Strings in Rust muss man auch die Zeichenkodierung beachten. Rust verwendet UTF-8, was Internationale Texte einfach macht, aber beim Ersetzen muss man darauf aufpassen, keine ungültigen Zeichenketten zu erzeugen.

## See Also
- Rust Buch - Strings: https://doc.rust-lang.org/book/ch08-02-strings.html
- `regex` Crate Dokumentation: https://docs.rs/regex/
- Online Regex Tester: https://regexr.com/
