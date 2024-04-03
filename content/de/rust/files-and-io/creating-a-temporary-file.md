---
date: 2024-01-20 17:41:07.153656-07:00
description: "So geht\u2019s: Um in Rust eine tempor\xE4re Datei zu erstellen, kannst\
  \ du die `tempfile` Crate nutzen. Hier ein Beispiel."
lastmod: '2024-03-13T22:44:53.692212-06:00'
model: gpt-4-1106-preview
summary: "Um in Rust eine tempor\xE4re Datei zu erstellen, kannst du die `tempfile`\
  \ Crate nutzen."
title: "Erstellung einer tempor\xE4ren Datei"
weight: 21
---

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
