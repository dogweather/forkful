---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Erstellen von temporären Dateien bezieht sich auf das Erzeugen von Dateien, die nur vorübergehend während einer Laufzeit einer Anwendung existieren. Programmierer tun dies, um nutzerspezifische Daten zwischenzuspeichern, die zu groß sind, um sie im Speicher zu behalten.

## So geht's:

Hier ist ein Beispiel, wie Sie eine temporäre Datei in Rust erstellen können:

```Rust
use std::fs::File;
use std::io::{Write, Error};
use tempfile::tempfile;

pub fn create_temp_file() -> Result<(), Error> {
    let mut file = tempfile()?;
    writeln!(file, "Hallo, temporäre Datei!")?;
    Ok(())
}
```

Wenn Sie diese Funktion ausführen, wird eine neue temporäre Datei erstellt und der Text "Hallo, temporäre Datei!" hinein geschrieben. Die Datei und ihr Pfad existieren nur solange, wie der `file`-Handle in Ihrem Programm existiert.

## Vertiefung

Historisch gesehen ermöglicht diese Methode das Überbrücken von Speicherbeschränkungen von Systemen, um Daten auszutauschen oder zu speichern, die mehr Speicher benötigen, als verfügbar ist. Alternativen zu dieser Methode können Vollständige "Ram-Disk"-Systeme sein, bei denen Daten vollständig im Systemspeicher gespeichert werden.

Die Erstellung der temporären Datei in Rust erfolgt sicher, d.h., die Datei wird mit einer zufällig generierten Dateinamen erstellt, der es für andere Anwendungen schwierig macht, darauf beabsichtigt zuzugreifen. Die `tempfile`-Funktion stellt dies sicher.

## Siehe auch

Hier sind einige Quellen, die Ihnen helfen könnten, mehr über dieses Thema zu erfahren

- Rust-Standardbibliothek: [std::fs](https://doc.rust-lang.org/std/fs/index.html)
- Tempfile-Dokumentation: [tempfile](https://docs.rs/tempfile/3.2.0/tempfile/)
- Eine großartige Diskussion über temporäre Dateien: [StackOverflow thread](https://stackoverflow.com/questions/60115476/create-a-temporary-file-in-rust)