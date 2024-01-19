---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Finden der Länge eines Strings ist ein Prozess, in dem wir zählen, wie viele Zeichen (einschließlich Leerzeichen) in einem String vorhanden sind. Programmierer tun dies oft, um die Anzahl der Benutzereingaben zu bestimmen oder um durch eine Sammlung von Zeichen zu iterieren.

## Wie:

Unten finden Sie ein kodierendes Beispiel für die Berechnung der Länge eines Strings in Rust:

```Rust
fn main() {
    let s = String::from("Hallo Welt");
    println!("Die Länge von '{}' ist {}.", s, s.len());
}
```

Wenn Sie das Programm ausführen, wird die Ausgabe sein:

```Rust
Die Länge von 'Hallo Welt' ist 11.
```

## Vertiefung

Historisch gesehen war die Suche nach der Länge einer Zeichenkette in vielen Programmiersprachen eine O(1) Operation. In Rust bleibt dies so, obwohl die Implementierung komplexer ist, da Rust Unicode-konform ist. Alternativ kann die Methode `chars().count()` verwendet werden, die die Anzahl der Unicode-Zeichen (anstatt der Bytes) zählt. Beachten Sie jedoch, dass `chars().count()` eine O(n) Operation ist und bei sehr langen Zeichenketten langsamer als `len()` sein kann.

## Siehe Auch

Weitere Links zu verwandten Ressourcen:

- Rust String-Dokumentation: https://doc.rust-lang.org/std/string/struct.String.html
- Rust by Example: https://doc.rust-lang.org/rust-by-example/std/str.html
- Die Rust Programming Language (Buch): https://www.nostarch.com/Rust