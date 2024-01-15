---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Rust: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Du hast vielleicht schon mal die Möglichkeit gesehen, in der Eingabeaufforderung eines Programmes Werte einzugeben. Diese Werte werden als Argumente bezeichnet und sind eine wichtige Methode, um die Funktionalität eines Programms anzupassen. In dieser Anleitung werde ich dir zeigen, wie du in Rust Befehlszeilenargumente lesen und verarbeiten kannst.

## Wie geht es

Das Lesen von Befehlszeilenargumenten in Rust ist ziemlich einfach. Zunächst musst du die Bibliothek `std::env` importieren, um auf die Funktionen zuzugreifen, die wir benötigen. Dann verwenden wir die Funktion `args()` aus dieser Bibliothek, um alle Befehlszeilenargumente in einem Vektor zu speichern. Hier ist ein Beispielcode:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("Das Programm befindet sich in der Datei: {}", args[0]);
    println!("Das erste Befehlszeilenargument lautet: {}", args[1]);
}
```

Diese Beispiele nutzen die Tatsache aus, dass der Vektor `args` den Namen der ausführbaren Datei als erstes Element enthält. Alle weiteren Argumente, die der Benutzer eingegeben hat, werden in der Reihenfolge in der sie eingegeben wurden, als darauf folgende Elemente des Vektors gespeichert. In der Ausgabe der beiden `println!()`-Anweisungen sehen wir, wie diese Elemente ausgegeben werden können.

Wenn du möchtest, kannst du auch die `for`-Schleife nutzen, um alle Argumente der Reihe nach auszugeben.

```Rust
for argument in args {
    println!("Das nächste Argument lautet: {}", argument);
}
```

Du kannst auch überprüfen, ob bei der Eingabe ein bestimmtes Argument angegeben wurde, indem du die Länge des Vektors mit den Argumenten vergleichst.

```Rust
if args.len() > 2 {
    println!("Es wurden mehr als zwei Argumente eingegeben.");
}
```

Um die Argumente in andere Datentypen zu konvertieren, musst du die Funktionen `parse()` und `unwrap()` nutzen. Hier ist ein Beispiel, um ein Argument in eine `u32` Zahl umzuwandeln.

```Rust
let num: u32 = args[1].parse().unwrap();
```

## Tiefentauchen

Wenn du mehr über das Verarbeiten von Befehlszeilenargumenten in Rust erfahren möchtest, gibt es einige wichtige Begriffe, die du kennen solltest. Zum Beispiel gibt es die `std::env::Args`-Struktur, mit der du auf einzelne Argumente zugreifen kannst, und die `std::env::args_os()`-Funktion, die die Argumente als `OsString`-Objekte zurückgibt.

Du kannst auch eine umfangreichere Bibliothek wie `clap` nutzen, die die Verarbeitung von Befehlszeilenargumenten vereinfacht und zusätzliche Funktionen wie die Unterstützung von Optionen und Unterbefehlen bietet.

## Siehe auch

- [Dokumentation zu Befehlszeilenargumenten in Rust](https://doc.rust-lang.org/std/env/index.html)
- [clap Bibliothek für Befehlszeilenargumente in Rust](https://github.com/clap-rs/clap)