---
title:                "Überprüfen, ob ein Verzeichnis vorhanden ist"
html_title:           "Rust: Überprüfen, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Überprüfen, ob ein Verzeichnis vorhanden ist, ist ein wichtiger Bestandteil der Programmierung. Es ermöglicht Entwicklern, sicherzustellen, dass bestimmte Dateien oder Ordner vorhanden sind, bevor sie darauf zugreifen oder damit arbeiten.

## Wie geht's:
In Rust gibt es die Funktion ```std::fs::metadata()```, mit der überprüft werden kann, ob ein bestimmtes Verzeichnis vorhanden ist. Sie gibt eine ```std::fs::Metadata``` Struktur zurück, die Informationen über die Datei oder das Verzeichnis enthält.

Beispielcode:
Rust
```
use std::fs;

fn main() {
    if let Ok(metadata) = fs::metadata("mein_verzeichnis") {
        if metadata.is_dir() {
            println!("Das Verzeichnis ist vorhanden!");
        }else{
            println!("Das Verzeichnis existiert nicht.");
        }
    }else{
        println!("Das Verzeichnis existiert nicht.");
    }
}
```

Beispieldatenausgabe:
```
Das Verzeichnis ist vorhanden!
```

## Tiefere Einblicke:
Die Funktionalität zum Überprüfen, ob ein Verzeichnis vorhanden ist, gibt es bereits seit den frühen Tagen der Programmierung. In Rust gibt es jedoch spezifische Funktionen, die nur für diese Aufgabe entwickelt wurden, was zu präziseren und effizienteren Ergebnissen führt.

Alternativ können Entwickler auch die "std::fs::read_dir()" Funktion verwenden, um alle Dateien und Verzeichnisse in einem bestimmten Ordner aufzulisten und somit auch zu überprüfen, ob ein bestimmtes Verzeichnis vorhanden ist.

Die Implementierung von "std::fs::metadata()" basiert auf dem Betriebssystem und der Dateisystem-Implementierung. Dies bedeutet, dass das Ergebnis je nach Plattform unterschiedlich sein kann.

## Weitere Informationen:
Rust Dokumentation zu "std::fs::metadata()": https://doc.rust-lang.org/std/fs/fn.metadata.html

Rust Dokumentation zu "std::fs::read_dir()": https://doc.rust-lang.org/std/fs/fn.read_dir.html

Diskussion zu "std::fs::metadata()" auf Stack Overflow: https://stackoverflow.com/questions/39631576/what-is-the-preferred-way-to-check-for-file-directory-and-read-write-permission