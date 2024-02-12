---
title:                "Lesen von Kommandozeilenargumenten"
aliases:
- /de/rust/reading-command-line-arguments.md
date:                  2024-01-20T17:56:59.414019-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lesen von Kommandozeilenargumenten"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?
Kommandozeilenargumente erlauben Nutzern, Programme durch zusätzliche Eingaben zu steuern. Programmierer nutzen sie, um flexiblere und anpassungsfähigere Tools zu erstellen, die unter verschiedenen Bedingungen und mit unterschiedlichen Daten arbeiten können.

## So geht’s:
Rust stellt die Struktur `std::env::args` zur Verfügung, die eine Sammlung der Kommandozeilenargumente bietet. Hier ist ein einfaches Beispiel:

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    for (index, argument) in args.iter().enumerate() {
        println!("Argument {}: {}", index, argument);
    }
}
```

Angenommen, du startest dein Programm mit `cargo run arg1 arg2`, wäre die Ausgabe:

```
Argument 0: target/debug/programmname
Argument 1: arg1
Argument 2: arg2
```

## Tiefgang
Historisch gesehen kommen Kommandozeilenargumente aus der Zeit der Terminal-Computer, wo Grafikoberflächen selten waren. Rust bietet eine moderne, sichere Möglichkeit, diese zu nutzen. Es gibt Alternativen, wie `clap`, `structopt` oder `getopts`, die ergonomischere Interfaces für komplexe Parameter bieten. `std::env::args` ignoriert das erste Argument (den Programmnamen), während `std::env::args_os` auch nicht-valides Unicode berücksichtigt. Sich mit den Implementationen hinter `std::env` zu befassen, lohnt sich, um zu verstehen, wie Rust mit OS-interfacing und Error handling umgeht.

## Siehe Auch
- [The Rust Programming Language – Command Line Arguments](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
- [std::env Module](https://doc.rust-lang.org/std/env/)
