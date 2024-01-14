---
title:                "Rust: Schreiben auf Standardfehler"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

In der Welt der Programmierung gibt es viele Situationen, in denen es hilfreich ist, eine Fehlermeldung in die Standardfehlerausgabe zu schreiben. Dies kann dazu beitragen, potenzielle Probleme frühzeitig zu erkennen und zu beheben, was letztendlich zu einem zuverlässigeren und stabileren Code führt.

## Wie geht man vor?

Das Schreiben in die Standardfehlerausgabe ist in der Programmierung oft eine einfache Aufgabe, aber es gibt einige wichtige Dinge zu beachten. Im Folgenden sind zwei Beispiele aufgeführt, wie man in Rust Code schreibt, der in die Standardfehlerausgabe schreibt:

```Rust
// Beispiel 1: Verwendung von `eprintln!`-Makro
fn main() {
    let error_message = "Something went wrong!";
    eprintln!("Error: {}", error_message);
}

// Beispiel 2: Verwendung von `std::io::stderr`-Modul
use std::io::Write;
fn main() {
    let mut stderr = std::io::stderr();
    let error_message = "Something went wrong!";
    writeln!(stderr, "Error: {}", error_message).expect("Failed to write to stderr");
}
```

Beide Beispiele erzielen dasselbe Ergebnis, aber es ist wichtig zu beachten, dass das erste Beispiel das `eprintln!`-Makro verwendet, während das zweite Beispiel das `std::io::stderr`-Modul verwendet. Beide Optionen sind gleichermaßen gültig, aber das Verständnis der Unterschiede kann dazu beitragen, die richtige Wahl für Ihren Code zu treffen.

Hier ist die Ausgabe unseres Beispielcodes:

```
Error: Something went wrong!
```

## Tiefere Einblicke

Das Schreiben in die Standardfehlerausgabe ist nicht nur hilfreich, um Probleme zu erkennen, sondern kann auch dazu beitragen, wichtige Informationen im Debugging-Prozess zu sammeln. Darüber hinaus ist es oft eine gängige Praxis, Fehlermeldungen in Protokolldateien zu schreiben, anstatt sie auf der Standardausgabe auszugeben. Dies kann dazu beitragen, ein saubereres und übersichtlicheres Output zu erhalten.

## Siehe auch

Hier sind einige nützliche Links, um Ihr Verständnis von Rust-Programmierung zu vertiefen:

- [Rust-Dokumentation](https://www.rust-lang.org/learn)
- [Standard Library Dokumentation](https://doc.rust-lang.org/std/index.html)
- [Fehlerbehandlung in Rust](https://doc.rust-lang.org/book/ch09-00-error-handling.html)