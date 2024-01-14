---
title:                "Rust: Verwendung von regulären Ausdrücken"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Warum

Wenn du schon eine Weile mit Rust programmiert hast, hast du wahrscheinlich schon von regulären Ausdrücken gehört. Aber warum sollte man sich überhaupt damit beschäftigen? Reguläre Ausdrücke sind äußerst nützlich, wenn es darum geht, Textmuster in Strings zu finden und zu manipulieren. Sie können dir helfen, komplexe Aufgaben schneller und effizienter zu erledigen.

# Wie

Um reguläre Ausdrücke in Rust zu nutzen, musst du das `regex` Modul in deine Datei importieren. Dann kannst du die `Regex` Struktur nutzen, um einen regulären Ausdruck zu definieren und mit ihm zu arbeiten.

```
Rust
use regex::Regex;

let text = "Hello, world!";
let regex = Regex::new(r"world").unwrap();
//Die `\.find()` Methode sucht in dem String nach dem regulären Ausdruck
if regex.find(text).is_some() {
    println!("Der String enthält den Text 'world'");
}
```

In diesem Beispiel definieren wir einen regulären Ausdruck, der nach dem Wort "world" sucht. Dann verwenden wir die `find()` Methode, um zu prüfen, ob der String diesen Text enthält. Die Ausgabe wäre "Der String enthält den Text 'world'".

# Tiefergehende Erklärung

Wenn du genauer verstehen möchtest, wie reguläre Ausdrücke in Rust funktionieren, gibt es mehrere Konzepte zu beachten. Erstens gibt es verschiedene Arten von regulären Ausdrücken, je nachdem, welchen "Dialekt" du verwendest. In Rust wird der Standard "ECMAScript" Dialekt verwendet, aber es gibt auch Optionen für "POSIX" und "Perl".

Außerdem gibt es verschiedene Symbole und Operatoren, die in regulären Ausdrücken verwendet werden, um Textmuster zu definieren. Zum Beispiel `.` für jedes beliebige Zeichen, `+` für ein oder mehrere Vorkommen eines Zeichens und `\d` für eine beliebige numerische Ziffer.

Es ist auch wichtig zu beachten, dass reguläre Ausdrücke immer auf Unicode-Text angewendet werden. Das bedeutet, dass sie nicht nur auf ASCII-Zeichen, sondern auch auf multibyte-Zeichen funktionieren.

# Siehe auch

- [Offizielle Rust-Dokumentation zu regulären Ausdrücken](https://doc.rust-lang.org/std/regex/)
- [Rust Regex-Bibliothek](https://crates.io/crates/regex)
- [Tutorial zu regulären Ausdrücken in Rust](https://dev.to/emmanuelantony2000/regular-expressions-in-rust-46c8)