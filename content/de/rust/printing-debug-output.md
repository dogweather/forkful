---
title:                "Debug-Ausgabe drucken"
html_title:           "Rust: Debug-Ausgabe drucken"
simple_title:         "Debug-Ausgabe drucken"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Debugging ist ein wichtiger Teil des Programmierens und das Drucken von Debug-Ausgaben ist oft eine nützliche Methode, um Probleme in unserem Code zu identifizieren. Mit Rust können wir dies auf verschiedene Arten tun, je nach unseren spezifischen Bedürfnissen.

## So geht's

Das Drucken von Debug-Ausgaben in Rust ist einfach und unkompliziert. Wir können `println!` oder `eprintln!` verwenden, um Ausgaben auf der Standardausgabe oder der standardmäßigen Fehlerausgabe zu drucken. Hier ist ein Beispiel:

```Rust
let num = 42;
println!("Die Nummer ist: {}", num);
```

Dieses Beispiel verwendet `println!` um den Wert von `num` auf der Standardausgabe auszudrucken. Wir können auch Formatierungsoptionen wie `{:?}` verwenden, um den vollständigen Inhalt eines Wertes zu drucken. Hier ist ein Beispiel:

```Rust
let name = "Peter";
println!("Der Name ist: {:?}", name);
```

Dies druckt den Wert von `name` auf der Standardausgabe in einem Debug-Format (z.B. "Peter" wird als `"Peter"` gedruckt). Alternativ können wir auch `dbg!` verwenden, um eine Debug-Ausgabe zu drucken und den Wert zurückzugeben. Hier ist ein Beispiel:

```Rust
let result = 4 + 2;
let sum = dbg!(result);
```

Dieses Beispiel nutzt `dbg!` um den Wert von `result` auf der Standardausgabe zu drucken und gleichzeitig den Wert an `sum` zuweisen.

## Tiefergehende Einblicke

In der Regel möchten wir Debug-Ausgaben nur in Entwicklungsumgebungen verwenden und nicht in der finalen Anwendung. Um dies zu erreichen, können wir das `debug`-Feature von Rust nutzen. Hier ist ein Beispiel:

```Rust
let num = 42;
#[cfg(debug_assertions)]
println!("DEBUG: Die Nummer ist: {}", num);
```

Dieses Beispiel nutzt die `#[cfg(debug_assertions)]`-Anweisung, um sicherzustellen, dass die entsprechende Debug-Ausgabe nur in Entwicklungsmodus kompiliert wird.

Ebenfalls wichtig ist, dass wir unsere Debug-Ausgaben richtig handhaben. Wenn wir `println!` oder `eprintln!` in einer Schleife verwenden, kann dies zu Leistungsproblemen führen, da sie für jede Ausgabe die Standardausgabe öffnen und schließen. In solchen Fällen sollten wir `io::stdout()` oder `io::stderr()` direkt verwenden und entsprechend mit einem `io::BufWriter` umwickeln. Hier ist ein Beispiel:

```Rust
use std::io::{self, Write};
let stdout = io::stdout();
let mut stdout_lock = BufWriter::new(stdout.lock());

for i in 1..=10 {
    write!(stdout_lock, "Iteration {}: {}\n", i, i * 2).unwrap();
}
```

Dieses Beispiel nutzt `io::BufWriter` um die Standardausgabe effizienter zu nutzen. Wir sollten auch immer bedenken, Debug-Ausgaben in unserem finalen Code zu entfernen, um unnötigen Overhead zu vermeiden.

## Siehe auch

- [Die offizielle Rust Dokumentation über Debug-Ausgaben](https://doc.rust-lang.org/std/macro.dbg.html)
- [Eine ausführliche Erklärung von Debug-Ausgaben in Rust](https://danielkeep.github.io/tlborm/book/blk-debug-assertions.html)