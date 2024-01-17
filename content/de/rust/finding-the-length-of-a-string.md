---
title:                "Die Länge eines Strings finden"
html_title:           "Rust: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Was & Warum?

Die Länge einer Zeichenkette zu finden bedeutet, die Anzahl der Zeichen in einer gegebenen Zeichenkette zu bestimmen. Programmierer tun dies, um Daten zu analysieren, Muster zu erkennen oder um die richtige Größe von Variablen oder Arrays zu bestimmen.

# Wie man es macht:

Um die Länge einer Zeichenkette in Rust zu finden, kann die Funktion `len()` des String-Typs verwendet werden. Diese Funktion gibt die Anzahl der Unicode-Skalare in der Zeichenkette zurück. Das folgende Beispiel zeigt, wie man die Länge einer Zeichenkette namens `my_string` findet:

```Rust
let my_string = "Hallo, Welt!";
println!("Die Länge meiner Zeichenkette ist {}", my_string.len());
```

Die Ausgabe wird sein:

```Rust
Die Länge meiner Zeichenkette ist 12
```

# Tiefere Einblicke:

Die Bestimmung der Länge von Zeichenketten ist eine grundlegende Operation in der Programmierung und wird in vielen Sprachen verwendet. In Rust gibt es keine spezielle Funktion, um die Länge einer Zeichenkette zu finden, da sie in die oben genannte Funktion `len()` des String-Typs integriert ist. Alternativ können Entwickler auch die Funktion `chars()` verwenden, um die Anzahl der Zeichen in einer Zeichenkette zu zählen.

Es ist wichtig zu beachten, dass bei der Bestimmung der Länge einer Zeichenkette in Rust jede Unicode-Kombination als ein einzelner Skalar betrachtet wird. Dies kann zu unterschiedlichen Ergebnissen führen als in anderen Sprachen, die auf anderen Codierungen basieren.

# Siehe auch:

- [Rust Dokumentation für `len()`](https://doc.rust-lang.org/std/string/struct.String.html#method.len)
- [Rust Dokumentation für `chars()`](https://doc.rust-lang.org/std/primitive.str.html#method.chars)