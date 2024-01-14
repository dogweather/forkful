---
title:                "Rust: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Warum Regular Expressions in Rust verwenden?

Wenn Sie eine effiziente Methode zur Mustererkennung in Texten benötigen, können Regular Expressions in Rust eine sehr nützliche Funktion sein. Durch ihre Kombination aus Algorithmen und Syntax können Sie Textmuster präzise definieren und so die Verarbeitung von Daten erleichtern.

## So funktionieren Regular Expressions in Rust

Um Regular Expressions in Rust zu verwenden, müssen Sie zunächst die Bibliothek "regex" in Ihrem Projekt importieren. Mit der Methode "regex::Regex::new()" können Sie dann ein Regex-Objekt erstellen, dem Sie anschließend mithilfe der Methode "is_match()" einen Text zur Analyse übergeben können. In folgendem Beispiel wird eine E-Mail-Adresse auf ihre Gültigkeit überprüft:

```rust
extern crate regex;
use regex::Regex;

fn main() {
    let email_regex = Regex::new(r"^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$").unwrap();
    let email = "example@mail.com";
    if email_regex.is_match(email) {
        println!("Die E-Mail-Adresse ist gültig!");
    } else {
        println!("Die E-Mail-Adresse ist ungültig!");
    }
}
```

Die Ausgabe in diesem Fall wäre: "Die E-Mail-Adresse ist gültig!". Das Regex-Muster enthält alle gängigen Regeln für E-Mail-Adressen und kann somit die Gültigkeit überprüfen.

## Tiefergehende Informationen zu Regular Expressions in Rust

Neben der einfachen Überprüfung von Texten können Sie mit Regular Expressions in Rust auch komplexere Aufgaben wie das Extrahieren von bestimmten Informationen aus einem Text lösen. Dazu können Sie mithilfe von Capture Groups bestimmte Teile eines Textes markieren und später darauf zugreifen. Mehr Informationen über Capture Groups und weitere fortgeschrittene Funktionen von Regular Expressions in Rust finden Sie in der offiziellen Dokumentation unter [https://docs.rs/regex/](https://docs.rs/regex/).

# Siehe auch

- Offizielle Dokumentation zu Regular Expressions in Rust: [https://docs.rs/regex/](https://docs.rs/regex/)
- Einführung in Regular Expressions in Rust von der offiziellen Rust-Website: [https://www.rust-lang.org/learn/regular-expressions](https://www.rust-lang.org/learn/regular-expressions)
- Weitere Beispiele für die Verwendung von Regular Expressions in Rust: [https://github.com/rust-lang/regex/tree/master/examples](https://github.com/rust-lang/regex/tree/master/examples)