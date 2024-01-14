---
title:                "Rust: Textsuche und Ersetzung"
simple_title:         "Textsuche und Ersetzung"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Der Textersetzung von Dateien ist eine häufige Aufgabe für Programmierer. Es kann nützlich sein, um Fehler in Code zu beheben, oder um große Textblöcke auf einmal zu ändern. Mit Rust gibt es eine effektive Möglichkeit, Text effizient zu suchen und zu ersetzen.

## Wie geht man vor

Um Text in Rust zu suchen und zu ersetzen, gibt es verschiedene Methoden. Eine Möglichkeit ist die Verwendung der Standardbibliothek "std::fs", um eine Datei zu öffnen und den Inhalt zu lesen. Dann kann man die Methode "replace" nutzen, um einen bestimmten Text durch einen anderen zu ersetzen.

```Rust
use std::fs;
let mut file = fs::File::open("beispiel.txt").expect("Datei konnte nicht geöffnet werden");
let mut inhalt = String::new();
file.read_to_string(&mut inhalt).expect("Fehler beim Lesen der Datei");

let neuer_inhalt = inhalt.replace("alt", "neu");
```

Mit diesem Codeblock wird der Text "alt" durch "neu" ersetzt und der neue Inhalt in der Variable "neuer_inhalt" gespeichert. Dann kann man den geänderten Inhalt einfach in eine Datei schreiben.

## Tiefere Einblicke

Das Ersetzen von Text in Rust kann auch mit Hilfe der Bibliothek "regex" durchgeführt werden. Diese ermöglicht die Verwendung von regulären Ausdrücken, um den zu ersetzenden Text zu definieren. Zudem bietet sie weitere Optionen, wie z.B. die Berücksichtigung von Groß- und Kleinschreibung oder die Verwendung von Variablen in dem zu ersetzenden Text.

```Rust
use regex::Regex;
let re = Regex::new(r"example").unwrap();
let neuer_inhalt = re.replace_all(&inhalt, "neues Beispiel");
```

Mit diesem Beispiel wird jeder Text, der "example" enthält, durch "neues Beispiel" ersetzt. Die Bibliothek "regex" bietet viele verschiedene Funktionen und Optionen, um Text in Rust zu manipulieren.

## Siehe auch

- [Dokumentation zu "std::fs" in Rust](https://doc.rust-lang.org/std/fs/)
- [Dokumentation zu der Bibliothek "regex" in Rust](https://docs.rs/regex/*/regex/index.html)