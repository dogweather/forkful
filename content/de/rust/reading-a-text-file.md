---
title:                "Eine Textdatei lesen"
html_title:           "Rust: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Lesen von Textdateien ist eine verbreitete Aufgabe für Programmierer, bei der sie den Inhalt einer Datei in ihrem Code verarbeiten und darauf zugreifen müssen. Dies kann zum Beispiel nützlich sein, um Daten in einem CSV-Format zu lesen oder um eine Konfigurationsdatei einzulesen.

## Wie geht's?

Rust bietet eine einfache Möglichkeit, Textdateien zu lesen, indem man die Standardbibliothek `std::fs::read_to_string` verwendet. Hier ist ein Beispiel:

```Rust
use std::fs;

fn main() {
    let file_contents = fs::read_to_string("beispiel.txt").expect("Datei konnte nicht gelesen werden");
    println!("{}", file_contents);
}
```

Der Inhalt der Datei "beispiel.txt" wird in der Variable `file_contents` gespeichert und dann mit `println!` auf der Konsole ausgegeben. Wenn die Datei nicht gefunden werden konnte oder ein anderer Fehler auftrat, wird eine entsprechende Fehlermeldung ausgegeben.

## Tiefgründiger Einblick

- **Historischer Kontext:** Das Lesen von Textdateien hat eine lange Geschichte in der Programmierung, da das Lesen von Benutzereingaben und die Verarbeitung von Dateien von Anfang an wichtige Aufgaben waren.

- **Alternativen:** Neben der Verwendung der `std::fs::read_to_string` Funktion gibt es noch andere Möglichkeiten, Textdateien in Rust zu lesen, wie z.B. die `File`-Struktur aus der `std::fs` Bibliothek oder die `BufReader`-Struktur aus der `std::io` Bibliothek.

- **Implementierungsdetails:** Die `read_to_string` Funktion verwendet eine Kombination aus dem `fs::File` Typ und dem `std::string::String` Typ, um den Dateiinhalt als String zurückzugeben. Sie kann auch mit der `fs::read` Funktion verwendet werden, um den Dateiinhalt in einem `Vec<u8>` zurückzugeben, falls dies für die Verarbeitung der Daten besser geeignet ist.

## Siehe auch

- [Dokumentation der `std::fs` Bibliothek in Rust](https://doc.rust-lang.org/std/fs/)
- [Beitrag über das Einlesen von CSV-Dateien in Rust](https://codereviewvideos.com/course/beginning-rust-programming/lesson/23-07-reading-csv-file-data)
- [Diskussion über die verschiedenen Optionen zum Lesen von Textdateien in Rust auf Stack Overflow](https://stackoverflow.com/questions/36308126/how-to-open-a-file-in-rust)