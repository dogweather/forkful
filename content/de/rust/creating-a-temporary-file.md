---
title:                "Erstellen einer temporären Datei"
html_title:           "Rust: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Was & Warum?
Das Erstellen einer temporären Datei ist eine häufige Aufgabe für Programmiererinnen und Programmierer. Dabei handelt es sich um eine Datei, die nur für die Dauer des Programms existiert und danach automatisch gelöscht wird. Dies wird oft genutzt, um temporäre Daten zu speichern oder um Dateien sicher zu verarbeiten.

Wie geht das:
```Rust
use std::fs::File;
use std::io::prelude::*;
use std::env;

fn main() {
    // Erstelle eine temporäre Datei mit dem Präfix "temp"
    let mut file = File::create(".temp").expect("Konnte temp Datei nicht erstellen");

    // Schreibe Daten in die Datei
    file.write_all(b"Hallo Welt!").expect("Konnte nicht in temp Datei schreiben");

    // Finde den Pfad der temporären Datei heraus
    let temp_path = env::current_dir().unwrap().join(".temp");
    println!("Temporäre Datei wurde erstellt unter: {}", temp_path.display());

    // Die Datei wird automatisch gelöscht, sobald das Programm beendet ist.
}
```

Tiefer eintauchen:
Temporäre Dateien werden schon seit Jahrzehnten von Programmierern genutzt, um Daten zwischenzuspeichern oder um Dateien sicher zu verarbeiten. Es gibt auch alternative Methoden, wie z.B. das Erstellen von Ordnern anstatt von Dateien oder die Nutzung von speziellen Modulen wie "tempfile" in Rust.

Es gibt verschiedene Möglichkeiten, temporäre Dateien in Rust zu erstellen. Einige Programmierer nutzen die Standardbibliothek "std::fs" wie im obigen Beispiel gezeigt, andere nutzen spezielle Bibliotheken wie "tempdir" oder "tempfile". Die genaue Implementierung hängt auch von den individuellen Bedürfnissen des Programms ab.

Weiterführende Links:
- Die offizielle Rust Dokumentation zur Erstellung von temporären Dateien: https://doc.rust-lang.org/std/fs/struct.File.html
- Eine Einführung in die "tempfile" Bibliothek: https://crates.io/crates/tempfile
- Eine Diskussion über die Nutzung von temporären Dateien in Rust: https://users.rust-lang.org/t/safe-creation-of-temporary-files/18451