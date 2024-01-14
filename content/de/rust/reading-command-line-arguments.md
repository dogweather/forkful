---
title:                "Rust: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals ein Programm geschrieben haben, das auf der Befehlszeile ausgeführt werden kann, haben Sie wahrscheinlich schon einmal benutzerdefinierte Argumente verwendet. Diese nützliche Funktion ermöglicht es dem Benutzer, bestimmte Einstellungen oder Funktionalitäten beim Ausführen des Programms anzupassen. In dieser Anleitung werden wir uns anschauen, wie man in Rust Befehlszeilenargumente liest und verarbeitet.

## Wie man Befehlszeilenargumente in Rust liest

Das Lesen von Befehlszeilenargumenten in Rust ist relativ einfach und erfordert nur wenige Zeilen Code. Wir verwenden die [`std::env` Bibliothek](https://doc.rust-lang.org/std/env/) um an die Argumente heranzukommen. Der folgende Codeausschnitt zeigt, wie man alle Argumente ausgibt:

```rust
use std::env;

fn main() {
  let args: Vec<String> = env::args().collect();
  for arg in args {
    println!("{}", arg);
  }
}
```

Der erste Eintrag im `args` Vektor ist immer der Name des Programms selbst. Wenn wir also unser Programm "hello_world" nennen, wird `args[0]` der String "hello_world" sein. Wir können auch auf bestimmte Argumente zugreifen, indem wir ihren Index übergeben. Zum Beispiel, um das dritte Argument auszugeben, würden wir `args[2]` verwenden. Der folgende Codeausschnitt zeigt, wie man das dritte Argument ausgibt, wenn es vorhanden ist, ansonsten eine Standardnachricht ausgibt:

```rust
use std::env;

fn main() {
  let args: Vec<String> = env::args().collect();
  
  let third_arg = args.get(2); // Index 2 entspricht dem dritten Argument
  
  match third_arg {
    Some(arg) => println!("{}", arg),
    None => println!("Kein drittes Argument vorhanden.")
  }
}
```

## Tiefere Einblicke

Es gibt noch weitere Möglichkeiten, Befehlszeilenargumente in Rust zu verarbeiten, wie z.B. das Parsen von Argumenten mit der [`clap` Bibliothek](https://crates.io/crates/clap) oder das Lesen von Environment-Variablen mit der [`std::env::var()`](https://doc.rust-lang.org/std/env/fn.var.html) Funktion. Es gibt auch viele verschiedene Möglichkeiten, wie Sie Ihre Argumente strukturieren und verarbeiten können, je nach den Anforderungen Ihres Programms.

## Siehe auch

- [`std::env`](https://doc.rust-lang.org/std/env/)
- [`clap`](https://crates.io/crates/clap)
- [`std::env::var()`](https://doc.rust-lang.org/std/env/fn.var.html)