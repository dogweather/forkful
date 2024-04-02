---
date: 2024-01-26 00:57:14.337443-07:00
description: "Fehlerbehandlung dreht sich darum, sich zu k\xFCmmern, wenn die Dinge\
  \ schiefgehen. Entwickler machen das, um das Unerwartete zu handhaben, und um\u2026"
lastmod: '2024-03-13T22:44:53.680095-06:00'
model: gpt-4-1106-preview
summary: "Fehlerbehandlung dreht sich darum, sich zu k\xFCmmern, wenn die Dinge schiefgehen.\
  \ Entwickler machen das, um das Unerwartete zu handhaben, und um\u2026"
title: Fehlerbehandlung
weight: 16
---

## Was & Warum?

Fehlerbehandlung dreht sich darum, sich zu kümmern, wenn die Dinge schiefgehen. Entwickler machen das, um das Unerwartete zu handhaben, und um sicherzustellen, dass ihre Rust-Programme robust sind und nicht einfach abstürzen, wenn sie auf ein Problem stoßen.

## Wie geht das:

Rust behandelt Fehler auf zwei wesentliche Arten: behebbare und unbehebbare Fehler. Lassen Sie uns beide betrachten.

Behebbare Fehler verwenden `Result<T, E>`:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("Datei erfolgreich geöffnet."),
        Err(_e) => println!("Datei konnte nicht geöffnet werden."),
    }
}
```

Die Ausgabe könnte je nach Ihrer `hello.txt` entweder "Datei erfolgreich geöffnet." oder "Datei konnte nicht geöffnet werden." sein.

Für unbehebbare Fehler verwenden wir `panic!`:

```Rust
fn main() {
    // Das wird das Programm zum Absturz bringen, weil die Datei wahrscheinlich nicht existiert.
    let _f = File::open("nowhere.txt").unwrap();
}
```

Führen Sie es aus, und Sie werden eine Panikmeldung sehen. Ihr Programm hält sofort an.

## Tiefergehend

Historisch gesehen war die Fehlerbehandlung in der Programmierung ein Durcheinander. Rust macht es richtig mit einer klaren Unterscheidung zwischen behebbaren und unbehebbaren Fehlern.

Das Enum `Result` ist für behebbare Fehler. Es ist explizit – man behandelt die Variante `Ok` oder `Err`. Es gibt Methoden wie `unwrap()` und `expect()` auch, aber das sind schnelle und schmutzige Abkürzungen, die zu einem `panic!` führen können.

`panic!` ist Rusts Art zu schreien, dass etwas wirklich Schlimmes passiert ist und es nicht bewältigen kann. Es ist wie ein unbehebbarer Fehler, der sofort die Ausführung stoppt. Ein Panik in Rust wird oft bei Bugs gespürt, von denen man nicht erwartet, sie zu behandeln, wie ein Indexieren außerhalb der Grenzen eines Arrays.

Fehlerbehandlung durch Rückgabe von `Result` wird bevorzugt, wenn Sie erwarten, mit Fehlern umzugehen. Es ist idiomatisches Rust, was bedeutet, es ist die Art und Weise, wie Rust-Entwickler vereinbart haben, die Dinge zu tun. Es gibt auch `Option<T>`, für Fälle, in denen ein Fehler einfach darin besteht, dass etwas `None` statt `Some(T)` ist. Es geht alles darum, das Unerwartete zu erwarten, ohne Angst.

Alternativen? Sicher, man könnte andere Fehlerbehandlungs-Crates für mehr Funktionen oder ergonomische Nutzung verwenden. Wie `anyhow` für einfache Fehlerbehandlung oder `thiserror` für Fehler in Bibliothekscode.

## Siehe Auch

Interessiert, tiefer einzutauchen? Hier ist, wo es weitergeht:

- [Rust Buch über Fehlerbehandlung](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - Ein großartiger Ort, um die Philosophie der Fehlerbehandlung in Rust zu verstehen.
- [Rust nach dem Vorbild: Fehlerbehandlung](https://doc.rust-lang.org/rust-by-example/error.html) - Interaktive Beispiele, um praktische Erfahrungen zu sammeln.

Denken Sie daran, gute Fehlerbehandlung ist nicht nur Codierung; es ist Fürsorge für die Benutzer Ihres Codes. Viel Spaß beim Codieren!
