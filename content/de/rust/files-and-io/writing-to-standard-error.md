---
aliases:
- /de/rust/writing-to-standard-error/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:26.156115-07:00
description: "Das Schreiben auf den Standardfehler (stderr) in Rust bedeutet, Fehlermeldungen\
  \ und Diagnosen getrennt vom Standardausgang (stdout) auf die Konsole zu\u2026"
lastmod: 2024-02-18 23:09:04.658956
model: gpt-4-0125-preview
summary: "Das Schreiben auf den Standardfehler (stderr) in Rust bedeutet, Fehlermeldungen\
  \ und Diagnosen getrennt vom Standardausgang (stdout) auf die Konsole zu\u2026"
title: Schreiben auf Standardfehler
---

{{< edit_this_page >}}

## Was & Warum?

Das Schreiben auf den Standardfehler (stderr) in Rust bedeutet, Fehlermeldungen und Diagnosen getrennt vom Standardausgang (stdout) auf die Konsole zu leiten. Programmierer tun dies, um normale Programmausgaben von Fehlermeldungen zu unterscheiden, was es einfacher macht, Fehler angemessen zu behandeln oder sie während der Ausführung in Logs oder Dateien umzuleiten.

## Wie:

Rust bietet eine unkomplizierte Möglichkeit, mit dem Makro `eprintln!` auf stderr zu schreiben, ähnlich wie `println!` für stdout verwendet wird. Hier ist ein einfaches Beispiel:

```rust
fn main() {
    eprintln!("Dies ist eine Fehlermeldung!");
}
```

Beispielausgabe (auf Standardfehler):
```
Dies ist eine Fehlermeldung!
```

Für mehr Kontrolle über die Fehlermeldungen, etwa wenn Sie Text formatieren oder I/O-Ergebnisse behandeln möchten, verwenden Sie die Funktion `stderr` aus dem Modul `std::io`. Diese Methode bietet einen Handle für den globalen stderr-Stream, auf den Sie dann mit Methoden wie `write_all` oder `writeln` aus dem `Write`-Trait schreiben können:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "Formatierte Fehlermeldung: {}", 404).expect("Fehler beim Schreiben auf stderr");
}
```

Beispielausgabe (auf Standardfehler):
```
Formatierte Fehlermeldung: 404
```

Wenn Sie in Umgebungen oder Anwendungen arbeiten, in denen Sie auf Bibliotheken für das Logging oder die Fehlerbehandlung angewiesen sind, sind Bibliotheken wie `log` und `env_logger` beliebt. Obwohl sie mehr für Loggingzwecke verwendet werden, sind sie konfigurierbar und können Fehlerebenen auf stderr leiten. Unten ist ein einfaches Beispiel für die Verwendung von `log` und `env_logger`:

Zuerst fügen Sie die Abhängigkeiten zu Ihrer `Cargo.toml` hinzu:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

Richten Sie dann das Logging in Ihrer Anwendung ein und verwenden Sie es:
```rust
fn main() {
    env_logger::init();
    log::error!("Dies ist eine Fehlermeldung, die auf stderr geloggt wird");
}
```

Wenn Sie dieses Programm ausführen (nachdem Sie `env_logger` mit einer geeigneten Umgebungsvariablen eingerichtet haben, zum Beispiel `RUST_LOG=error`), wird die Fehlermeldung auf stderr ausgegeben, wobei die Logging-Infrastruktur genutzt wird.

```plaintext
ERROR: Dies ist eine Fehlermeldung, die auf stderr geloggt wird
```
