---
title:                "Schreiben auf Standardfehler"
date:                  2024-01-19
html_title:           "Arduino: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Schreiben auf den Standardfehler (stderr) separiert Fehlermeldungen von regulärem Output. Es erleichtert die Log-Analyse und die Weiterleitung von Fehlern zu Diagnosewerkzeugen.

## How to:
```Rust
use std::io::{self, Write};

fn main() {
    // Schreiben einer einfachen Fehlermeldung auf stderr
    writeln!(io::stderr(), "Fehler: Eine Datei konnte nicht geöffnet werden.").unwrap();

    // Ausgabe bei Erfolg auf stdout und bei Fehlern auf stderr
    if let Err(e) = do_something() {
        writeln!(io::stderr(), "Fehler beim Ausführen der Aktion: {}", e).unwrap();
    }
}

fn do_something() -> Result<(), io::Error> {
    // Code, der eine Aktion ausführt und einen Fehler zurückgeben könnte
    Err(io::Error::new(io::ErrorKind::Other, "etwas ist schiefgelaufen"))
}
```
Ausgabe:
```
Fehler: Eine Datei konnte nicht geöffnet werden.
Fehler beim Ausführen der Aktion: etwas ist schiefgelaufen
```

## Deep Dive:
Stderr wurde konzipiert, um Fehlermeldungen zu trennen, was vor allem in Unix-Systemen nützlich ist. Alternativ könnten Logger-Bibliotheken verwendet werden, aber stderr ist ein einfacher, integrierter Weg. In Rust greift man auf stderr über das `std::io` Modul zu und kann messengerspezifische Writer implementieren, um die Funktionalität zu erweitern.

## See Also:
- [Rust Standard Library std::io](https://doc.rust-lang.org/std/io/index.html)
- [Unix-Philosophie](https://de.wikipedia.org/wiki/Unix-Philosophie)
- [Rust-Buch über Fehlerbehandlung](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
