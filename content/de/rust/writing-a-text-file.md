---
title:                "Das Schreiben einer Textdatei"
html_title:           "Rust: Das Schreiben einer Textdatei"
simple_title:         "Das Schreiben einer Textdatei"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben einer Textdatei ist eine gängige Aufgabe für Programmierer:innen. Es bedeutet einfach, dass wir einen Text in einer Datei speichern, damit er später von unserem Code gelesen oder bearbeitet werden kann. Programmierer:innen schreiben Textdateien, um Daten zu speichern, Konfigurationen zu erstellen oder Ergebnisse von Programmen zu speichern.

## Anleitung:
Hier ist ein Beispiel in Rust, wie man eine Textdatei mit einem einfachen Textinhalt erstellt:
```Rust
use std::fs::File;
use std::io::Write;

fn main() {
    let mut file = File::create("output.txt").expect("Datei konnte nicht erstellt werden.");
    file.write_all(b"Hallo, Welt!").expect("Konnte Inhalt nicht schreiben.");
}
```

Die letzte Zeile schreibt den Text "Hallo, Welt!" in die Datei "output.txt" und gibt eine Fehlermeldung aus, wenn etwas schief geht. Um den Inhalt einer Textdatei zu lesen, können wir die Funktion ```File::open``` verwenden:
```Rust
use std::fs::File;
use std::io::prelude::*; // Für die Funktion `read_to_string`

fn main() {
    let mut file = File::open("input.txt").expect("Datei konnte nicht geöffnet werden.");
    let mut content = String::new();
    file.read_to_string(&mut content).expect("Konnte Inhalt nicht lesen.");
    
    println!("{}", content); // Gibt den Inhalt der Datei auf der Konsole aus.
}
```

## Tiefer Einblick:
Das Schreiben von Textdateien ist eine grundlegende und wichtige Fähigkeit für Programmierer:innen. Es gibt jedoch auch alternative Methoden, um Daten zu speichern, z.B. das Schreiben in eine Datenbank oder das Verwenden von JSON-Dateien. Die Implementierung von Textdateien in Rust ist dank der Standardbibliothek ```std::fs``` relativ einfach und intuitiv. Es ist auch möglich, mit der externen Bibliothek ```serde``` automatisch komplexe Datentypen in Textdateien zu serialisieren und zu deserialisieren.

## Weitere Informationen:
- [Rust Dokumentation zu Dateien und Verzeichnissen](https://doc.rust-lang.org/std/fs/index.html)
- [Rust Standardbibliothek: Schreiben in Dateien](https://doc.rust-lang.org/std/fs/struct.File.html#method.write_all)
- [Rust Standardbibliothek: Lesen von Dateien](https://doc.rust-lang.org/std/fs/struct.File.html#method.read_to_string)
- [serde Bibliothek](https://docs.rs/serde/1.0.118/serde/index.html)