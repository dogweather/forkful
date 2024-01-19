---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

---

# In Rust eine Textdatei lesen: Ein Leitfaden
---
## Was & Warum?
Eine Textdatei zu lesen bedeutet, den Inhalt einer Datei in deinem Programm zu holen und zu manipulieren. Es ist hilfreich, um von Benutzern bereitgestellte Daten zu verarbeiten oder um Informationen zwischen verschiedenen Arbeitssitzungen zu speichern.

## So geht's:
```Rust
use std::fs::File;
use std::io::{self, prelude::*, BufReader};

fn main() -> io::Result<()> {
    let file = File::open("path/to/your/file")?;
    let reader = BufReader::new(file);

    for line in reader.lines() {
        println!("{}", line?);
    }

    Ok(())
}
```
Wenn du dieses Code-Snippet ausführst, wirst du die Ausgabe jeder Zeile deiner Datei sehen.

## Tief tauchen
Historisch gesehen setzen sich Datei-Lesevorgänge aus niedrigleveligen Betriebssystemaufrufen wie `open`, `read` und `close` zusammen. Rust abstrahiert diese Aufrufe jedoch in den `File`- und `BufReader`-Typen, um die Arbeit zu erleichtern und Fehler zu reduzieren. 

Es gibt auch Alternativen zum Lesen von Dateien. Du könntest `mmap` verwenden, um die Datei in den Speicher zu mappen, oder ein Archivierungsformat wie `tar` oder `zip` nutzen, um mehrere Dateien zusammenzufassen.

Die Implementierung in Rust macht Gebrauch von Traits, wie `Read` und `BufRead`, um eine flexible und effiziente Methode zum Einlesen von Dateien zu ermöglichen.

## Siehe auch
Für weitere Informationen, hier einige zusätzliche Ressourcen:

- Rust Dokumentation über Dateiverarbeitung: https://doc.rust-lang.org/book/ch12-02-reading-a-file.html
- Der `Read` Trait in der Rust-Dokumentation: https://doc.rust-lang.org/std/io/trait.Read.html
- Der `BufRead` Trait in der Rust-Dokumentation: https://doc.rust-lang.org/std/io/trait.BufRead.html