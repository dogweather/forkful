---
title:                "Rust: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Das Schreiben nach `standard error` ist wichtig, um Fehler und Warnungen in unserem Programm zu verfolgen und zu debuggen. Es ermöglicht uns auch, wichtige Informationen während der Laufzeit unseres Programms anzuzeigen.

## Wie man

Zunächst müssen wir das `std`-Modul von Rust importieren, um auf die Funktionen für das Schreiben nach `standard error` zugreifen zu können. Dann können wir die `eprint!` oder `eprintln!` Makros verwenden, um Text an den `standard error` zu schreiben.

 ```Rust
use std::io::{self, Write};

fn main() {
    // Schreiben nach standard error mit eprint!
    eprint!("Dies ist eine Fehlermeldung.");

    // Schreiben nach standard error mit eprintln!
    eprintln!("Dies ist eine Warnung: {}", "Out of Memory");
}
 ```
Das obige Beispiel verwendet die `eprint!` und `eprintln!` Makros, um Text direkt nach `standard error` zu schreiben. Es ist jedoch auch möglich, einen `io::Error` zu erzeugen und ihn mit der `eprint!` oder `eprintln!` Makros auszugeben.

```Rust
 use std::io::{self, Write};

fn main() {
    // Erstelle einen io::Error
    let error = io::Error::new(io::ErrorKind::Other, "Out of Memory");

    // Gib den Fehler nach standard error aus
    eprint!("Ein Fehler ist aufgetreten: {}", error);
}
```

## Tieferer Einblick

Das Schreiben nach `standard error` unterscheidet sich vom Schreiben nach `standard output` in einigen wichtigen Punkten. Zum einen ist `standard error` ungepuffert, was bedeutet, dass die Ausgabe sofort auf dem Bildschirm erscheint, auch wenn das Programm noch läuft. `standard output` ist hingegen gepuffert, was bedeutet, dass die Ausgabe möglicherweise nicht sofort angezeigt wird.

Darüber hinaus ist `standard error` standardmäßig rot gefärbt, um es von `standard output` zu unterscheiden. Dies kann jedoch von der Umgebung, in der das Programm ausgeführt wird, geändert werden.

## Siehe auch
- [Rust-Dokumentation für das std-Modul](https://doc.rust-lang.org/std/index.html)
- [Offizielle Rust-Website](https://www.rust-lang.org/de-DE/)
- [Fehlerbehandlung in Rust](https://doc.rust-lang.org/book/ch09-00-error-handling.html)