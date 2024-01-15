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

## Warum

Du fragst dich vielleicht, warum jemand überhaupt eine temporäre Datei erstellen würde. Nun, es gibt viele mögliche Gründe. Zum Beispiel könnte es sein, dass du während der Ausführung deines Programms temporäre Daten speichern musst, die später nicht mehr benötigt werden. Oder dein Programm arbeitet mit sensiblen Daten und du möchtest vermeiden, dass diese dauerhaft auf der Festplatte gespeichert werden. In diesen Fällen ist die Erstellung einer temporären Datei eine nützliche und sichere Lösung.

## Wie geht man vor?

In Rust gibt es eine Standardbibliotheksfunktion namens `std::fs::File::create`, die verwendet werden kann, um eine temporäre Datei zu erstellen. Hier ist ein Beispielcode:

```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    // Temporäre Datei erstellen
    let tmp_file = File::create("temp.txt").expect("Konnte die Datei nicht erstellen.");

    // In die Datei schreiben
    let data = b"Hallo Welt!";
    tmp_file.write_all(data).expect("Konnte nicht in die Datei schreiben.");

    // Datei schließen
    drop(tmp_file);
}
```

In diesem Beispiel haben wir eine temporäre Datei mit dem Namen "temp.txt" erstellt und den Text "Hallo Welt!" in die Datei geschrieben. Anschließend wird die Datei geschlossen, indem wir die `drop`-Funktion aufrufen, um sicherzustellen, dass sie nach der Verwendung gelöscht wird.

## Tiefer in die Materie einsteigen

Bei der Erstellung einer temporären Datei gibt es einige Dinge zu beachten. Zum Beispiel ist es wichtig, sicherzustellen, dass die Datei nach der Verwendung gelöscht wird, um Speicherplatz freizugeben und Datenschutz zu gewährleisten. Dies kann erreicht werden, indem man die `drop`-Funktion oder das `tempfile`-Crate verwendet, das speziell für die Erstellung und Verwaltung temporärer Dateien in Rust entwickelt wurde.

Eine weitere wichtige Überlegung ist die Sicherheit. Da temporäre Dateien oft sensible Daten enthalten können, ist es wichtig, sicherzustellen, dass nur autorisierte Benutzer Zugriff auf diese Dateien haben. In Rust kann dies erreicht werden, indem man den richtigen Dateimodus verwendet und sicherstellt, dass die Datei nur vom aktuellen Benutzer gelesen und geschrieben werden kann.

## Siehe auch

- [Dokumentation zu `std::fs::File::create`](https://doc.rust-lang.org/std/fs/struct.File.html#method.create)
- [Dokumentation zu `std::io::prelude`](https://doc.rust-lang.org/std/io/prelude/index.html)
- [`tempfile` Crate](https://crates.io/crates/tempfile)