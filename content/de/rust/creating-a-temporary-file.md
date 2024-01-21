---
title:                "Erstellung einer temporären Datei"
date:                  2024-01-20T17:41:07.153656-07:00
model:                 gpt-4-1106-preview
simple_title:         "Erstellung einer temporären Datei"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erstellen einer temporären Datei ermöglicht die vorübergehende Speicherung von Daten während der Ausführung eines Programms. Programmierer nutzen sie für Dinge wie das sichere Speichern von Geheimnissen, das Zwischenspeichern von Informationen oder das Verarbeiten von Daten, die nicht dauerhaft aufbewahrt werden müssen.

## So geht’s:

Um in Rust eine temporäre Datei zu erstellen, kannst du die `tempfile` Crate nutzen. Hier ein Beispiel:

```Rust
use tempfile::NamedTempFile;
use std::io::{Write, Read};

fn main() -> std::io::Result<()> {
    let mut temp_file = NamedTempFile::new()?;
    write!(temp_file, "Hallo, Welt!")?;

    let mut inhalt = String::new();
    temp_file.as_file_mut().rewind()?;
    temp_file.read_to_string(&mut inhalt)?;
    println!("Temporäre Datei Inhalt: {}", inhalt);
    Ok(())
}
```

Sample Output:

```
Temporäre Datei Inhalt: Hallo, Welt!
```

## Deep Dive

Temporäre Dateien sind so alt wie die Computerei selbst. Sie sind hilfreich, um Ergebnisse von Berechnungen zu speichern oder Daten auszutauschen, ohne dauerhaft Ressourcen zu binden. In Rust kümmert sich die `tempfile` Crate um das Erstellen und Verwalten dieser flüchtigen Dateien. Sie stellt sicher, dass Dateien einzigartige Namen erhalten und nach Gebrauch gelöscht werden, um die Privatsphäre zu schützen und Speicherplatz freizugeben. Temporäre Dateien liegen in einem Betriebssystem-spezifischen Temp-Verzeichnis. Alternativen zur `tempfile` Crate sind direkte Zugriffe auf das Dateisystem, was allerdings mehr Arbeit bedeutet und fehleranfälliger ist.

## Siehe Auch

- Die `tempfile` Crate Dokumentation: https://docs.rs/tempfile/
- Rust Programmierhandbuch zum Umgang mit Dateien: https://doc.rust-lang.org/book/ch12-00-an-io-project.html
- Rust Standardbibliothek zum Thema `File`: https://doc.rust-lang.org/std/fs/struct.File.html