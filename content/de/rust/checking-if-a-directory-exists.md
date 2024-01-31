---
title:                "Überprüfung, ob ein Verzeichnis existiert"
date:                  2024-01-20T14:58:45.579307-07:00
html_title:           "Fish Shell: Überprüfung, ob ein Verzeichnis existiert"
simple_title:         "Überprüfung, ob ein Verzeichnis existiert"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Prüfen, ob ein Verzeichnis existiert, bedeutet, zu kontrollieren, ob ein bestimmter Pfad auf dem Dateisystem zu einem Verzeichnis führt. Programmierer machen das, um Fehler zu vermeiden und entscheiden zu können, ob weitere Operationen, wie das Lesen aus oder Schreiben in das Verzeichnis, möglich sind.

## How to:
In Rust nutzt man das `std::fs` Modul, um mit dem Dateisystem zu arbeiten. Hier zwei Wege, um die Existenz eines Verzeichnisses zu prüfen:

```Rust
use std::path::Path;

fn main() {
    let path = Path::new("/ein/beispiel/pfad");

    if path.exists() && path.is_dir() {
        println!("Das Verzeichnis existiert.");
    } else {
        println!("Das Verzeichnis existiert nicht.");
    }
}
```

Die Ausgabe wäre entweder `Das Verzeichnis existiert.` oder `Das Verzeichnis existiert nicht.`, je nach Zustand des Pfads.

## Deep Dive
Die Verwendung der `Path`- und `PathBuf`-Strukturen für Pfadoperationen ist seit Rust 1.0 so designet. Alternativ könnten wir die `metadata()`-Funktion verwenden, müssen dann aber Fehlerbehandlung selbst machen. Im Gegensatz dazu kombiniert `path.exists()` die Fehlerprüfung direkt.

Alternativer Weg mit `metadata()`:
```Rust
use std::fs;

fn main() {
    let path = "/ein/beispiel/pfad";

    match fs::metadata(path) {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("Das Verzeichnis existiert.");
            } else {
                println!("Existiert, ist aber keine Verzeichnis.");
            }
        },
        Err(_) => println!("Das Verzeichnis existiert nicht."),
    }
}
```

Die Entscheidung zwischen `path.exists()` und `metadata()` hängt oft von der benötigten Fehlergranularität ab.

## See Also
- Rust Documentation: [Path](https://doc.rust-lang.org/std/path/struct.Path.html) und [fs::metadata](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- Rust by Example: [Filesystem Operations](https://doc.rust-lang.org/rust-by-example/std_misc/fs.html)
