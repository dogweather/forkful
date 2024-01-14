---
title:                "Rust: Das Lesen von Befehlszeilenargumenten"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Manchmal ist es nützlich, ein Programm mit zusätzlichen Informationen zu starten, die direkt von der Kommandozeile eingegeben werden können. In Rust können diese Argumente einfach gelesen und in das Programm integriert werden.

## Wie geht man vor

Um Kommandozeilenargumente in Rust zu lesen, müssen wir die `std::env` Bibliothek importieren. Dann können wir die Funktion `args()` verwenden, um eine Liste der Argumente zu erhalten. Wir können diese Liste durchlaufen und jeden Wert für unsere Zwecke nutzen.

```Rust
use std::env;
let args: Vec<String> = env::args().collect();
for arg in args.iter() {
    println!("{}", arg);
}
```

Wenn wir dieses Stück Code ausführen, werden alle Argumente, die beim Start des Programms angegeben wurden, ausgegeben. Wenn wir das Programm beispielsweise mit dem Befehl `rustc main.rs` kompilieren und dann mit dem Befehl `./main argument1 argument2` ausführen, würde die Ausgabe folgendermaßen aussehen:

```sh
./main
argument1
argument2
```

## Tieferer Einblick

Die Funktion `args()` gibt uns eine `std::env::Args` Struktur zurück, die eine Iterator-ähnliche Schnittstelle bereitstellt. Das bedeutet, dass wir nicht nur über die Argumente iterieren können, sondern auch andere nützliche Methoden wie `nth()` oder `next()` verwenden können. Außerdem gibt es auch die Möglichkeit, einen spezifischen Index des Arguments zu extrahieren, indem wir die Methode `nth()` mit der Indexnummer aufrufen.

Ein weiteres nützliches Feature dieser Struktur ist die Möglichkeit, den Namen des Programms zu erhalten, das ausgeführt wurde. Wir können dies tun, indem wir die Methode `next()` aufrufen, da der erste Eintrag in der Liste normalerweise der Name des Programms ist.

```Rust
use std::env;
let args: Vec<String> = env::args().collect();
let program_name = args[0].clone();

let mut args = env::args();
args.next();

for arg in args {
    println!("{}", arg);
}

println!("Programm: {}", program_name);
```

Die Ausgabe dieses Codes wäre folgende:

```sh
argument1
argument2
Programm: ./main
```

## Siehe auch

- [Rust-Dokumentation: Kommandozeilen-Argumente lesen](https://doc.rust-lang.org/std/env/fn.args.html)
- [Der offizielle Rust-Blog](https://blog.rust-lang.org/)