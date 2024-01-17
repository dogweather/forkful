---
title:                "Erzeugung von Zufallszahlen"
html_title:           "Rust: Erzeugung von Zufallszahlen"
simple_title:         "Erzeugung von Zufallszahlen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Generieren von Zufallszahlen ist eine gängige Programmieraufgabe, bei der eine zufällige Zahl innerhalb eines bestimmten Bereichs erstellt wird. Programmierer verwenden dies, um zufällige Entscheidungen zu treffen, Spiele zu erstellen, bestimmte Testfälle zu generieren und vieles mehr.

## Wie geht's?
Die `Rand`-Crate in Rust bietet eine einfache Möglichkeit, Zufallszahlen zu generieren. Nachdem diese Crate in Ihr Projekt eingebunden wurde, können Sie die Methode `thread_rng().gen_range()` verwenden, um eine zufällige Zahl in einem bestimmten Bereich zu generieren. Hier ist ein Beispiel:
```
use rand::Rng;

fn main() {
    let mut rng = rand::thread_rng();
    println!("Eine zufällige Zahl zwischen 1 und 10: {}", rng.gen_range(1, 11));
}
```
Die Ausgabe könnte beispielsweise `Eine zufällige Zahl zwischen 1 und 10: 7` sein.

## Tiefergehende Informationen
Das Generieren von Zufallszahlen ist eine wichtige Aufgabe in der Programmierung und hat eine lange Geschichte. Früher wurden Zufallszahlen durch komplexe mathematische Algorithmen erzeugt, aber heutzutage gibt es viele moderne Bibliotheken und APIs, die diese Aufgabe vereinfachen. In Rust wird die Methode `thread_rng()` von der standardmäßig eingebundenen `rand`-Crate verwendet, die auf der C-Bibliothek `librand` basiert.

## Siehe auch
- [Die offizielle Dokumentation zur `Rand`-Crate in Rust](https://docs.rs/rand/0.7.3/rand/)
- [Weitere Möglichkeiten, Zufallszahlen in Rust zu generieren](https://www.geeksforgeeks.org/generating-random-numbers-in-rust/)