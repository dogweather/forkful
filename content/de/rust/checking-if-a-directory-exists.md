---
title:                "Überprüfung, ob ein Verzeichnis existiert"
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Überprüfen, ob ein Verzeichnis existiert, bezeichnet den Prozess, bei dem festgestellt wird, ob ein bestimmter Verzeichnispfad auf Ihrem Computersystem existiert. Programmierer tun dies, um Fehler zu vermeiden, die auftreten, wenn versucht wird, auf ein nicht existierendes Verzeichnis zuzugreifen.

## So Geht's:

Mit Rust können wir diese Überprüfung einfach durchführen. Hier ist ein Beispiel:

```Rust
use std::path::Path;

fn main() {
    let path = Path::new("./pfad/zu/deinem/verzeichnis");
  
    if path.exists() {
        println!("Das Verzeichnis existiert!");
    } else {
        println!("Das Verzeichnis existiert nicht!");
    }
}
```

Bei Ausführung dieses Codes erhalten Sie:

```Rust
"Das Verzeichnis existiert!" // wenn das Verzeichnis existiert.
"Das Verzeichnis existiert nicht!" // wenn das Verzeichnis nicht existiert.
```

## Deep Dive:

Historisch gesehen konnte man in älteren Programmiersprachen wie C das Vorhandensein eines Verzeichnisses überprüfen, indem man versuchte, es zu öffnen und den resultierenden Fehler zu überprüfen. Rust bietet jedoch eine saubere und effiziente Methode zum Überprüfen, ob ein Verzeichnis existiert, ohne das Risiko von Ausnahmefehlern.

Eine Alternative zur Verwendung der Methode `exists()` könnte die Verwendung der Methode `metadata()` sein, die mehr Informationen über das Verzeichnis liefert, einschließlich seiner Existenz.

Tiefgreifender gesehen, nutzt `Path::exists()` intern die Funktion `metadata()`. Die Methode `exists()` gibt einfach ein `true` oder `false` zurück, basierend auf dem Erfolg der `metadata()`-Operation.

## Siehe Auch:

- Rust Dokumentation zum Path Modul: <https://doc.rust-lang.org/std/path/>
- Rust Dokumentation zur Methode `metadata()`: <https://doc.rust-lang.org/std/fs/fn.metadata.html>
- Weitere Einzelheiten zur Funktion `Path::exists()`: <https://doc.rust-lang.org/std/path/struct.Path.html>