---
title:                "Rust: Erstellen einer temporären Datei"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

##Warum

Das Erstellen von temporären Dateien kann in vielen Anwendungsfällen nützlich sein. Zum Beispiel kann es hilfreich sein, um temporäre Daten zu speichern, während ein Programm läuft, oder um vorübergehend Daten zwischen verschiedenen Prozessen auszutauschen. In Rust gibt es verschiedene Möglichkeiten, um temporäre Dateien zu erstellen, die in diesem Blogbeitrag näher erläutert werden.

##Wie man in Rust temporäre Dateien erstellt

Um in Rust eine temporäre Datei zu erstellen, gibt es mehrere Optionen. Eine Möglichkeit ist die `tempfile`-Bibliothek, die vom Rust-Team bereitgestellt wird.

```Rust
use tempfile::NamedTempFile;

let mut temp_file = NamedTempFile::new().expect("Konnte keine temporäre Datei erstellen");
let file_path = temp_file.path();

println!("Dateipfad: {:?}", file_path);
```

Dieses Beispiel importiert die `NamedTempFile`-Struktur aus der `tempfile`-Bibliothek und erstellt dann eine temporäre Datei mit der `new()`-Methode. Mit der `path()`-Methode können wir den Pfad zur erstellten temporären Datei abrufen und diesen dann beispielsweise für weitere Operationen verwenden.

Eine weitere Möglichkeit, temporäre Dateien in Rust zu erstellen, ist über die `fs`-Bibliothek des Standardpakets.

```Rust
use std::fs::File;
use std::io::prelude::*;

let mut temp_file = File::create("temp.txt").expect("Konnte temporäre Datei nicht erstellen");

temp_file.write_all(b"Dies ist eine temporäre Datei").expect("Konnte nicht in Datei schreiben");
```

Dieses Beispiel erstellt eine temporäre Datei mit dem Namen "temp.txt" und schreibt dann einen Text in diese Datei.

##Eintauchen in die Erstellung von temporären Dateien

Beim Erstellen von temporären Dateien gibt es einige Dinge zu beachten. Zum einen sollten wir sicherstellen, dass wir die Datei nach ihrer Verwendung wieder löschen, um Ressourcen zu sparen und mögliche Konflikte zu vermeiden. Dazu können wir die `unlink()`-Methode verwenden.

```Rust
use tempfile::NamedTempFile;

let mut temp_file = NamedTempFile::new().expect("Konnte keine temporäre Datei erstellen");

// Hier können wir Operationen mit der Datei durchführen

temp_file.unlink().expect("Konnte Datei nicht löschen");
```

Zum anderen ist es wichtig zu beachten, dass temporäre Dateien automatisch gelöscht werden, wenn sie ihren Scope verlassen. Das bedeutet, dass wir in Rust keine explizite Methode zum Löschen von temporären Dateien verwenden müssen, es sei denn, wir möchten dies ausdrücklich tun.

##Weitere Ressourcen

* [Dokumentation zu tempfile](https://docs.rs/tempfile/3.1.0/tempfile/)
* [Beispiel für die Erstellung von temporären Dateien in Rust](https://gist.github.com/leshow/25fdb37bd4e232b74ba1)
* [Artikel über das Erstellen und Verwalten von temporären Dateien in Rust](https://stevedonovan.github.io/rust-gentle-intro/6-temp-files.html)

##Siehe auch

* [Rust Dokumentation auf Deutsch](https://www.rust-lang.org/de-DE/)
* [Rust Beginners Guide auf Deutsch](https://www.besterkirchberg.de/rust/doc/Rust-Introduction-A-Deutsche-Einführung/8/index.html)