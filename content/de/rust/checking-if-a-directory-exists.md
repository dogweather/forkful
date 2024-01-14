---
title:    "Rust: Überprüfung, ob ein Verzeichnis existiert"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

 Die Überprüfung, ob ein Verzeichnis existiert, ist ein wichtiger Aspekt der Programmierung in Rust. Es hilft dabei, sicherzustellen, dass die erforderlichen Dateien und Ordner vorhanden sind, bevor das Programm ausgeführt wird. Außerdem ermöglicht es eine reibungslose Ausführung und verhindert potenzielle Fehler und Abstürze.

## Anleitung

Um zu überprüfen, ob ein Verzeichnis in Rust existiert, können Sie die Funktion `std::fs::metadata()` verwenden. Diese Funktion gibt eine `std::fs::Metadata` Struktur zurück, die Informationen über die Datei oder den Ordner enthält.

```Rust
use std::fs;

match fs::metadata("Pfad/zum/Verzeichnis") {
    Ok(metadata) => {
        if metadata.is_dir() {
            println!("Das Verzeichnis existiert.");
        } else {
            println!("Das angegebene Pfad ist kein Verzeichnis.");
        }
    }
    Err(error) => println!("Fehler beim Abrufen von Metadaten: {}", error),
}
```

Dieses Beispiel verwendet die `is_dir()` Methode, um zu überprüfen, ob die übermittelte Datei oder der übermittelte Ordner tatsächlich ein Verzeichnis ist. Sie können auch die `is_file()` Methode verwenden, um zu überprüfen, ob es sich um eine Datei handelt.

Im Falle eines Fehlers, z.B. wenn der angegebene Pfad nicht gefunden werden kann, wird eine `Err` Struktur zurückgegeben und eine Fehlermeldung ausgegeben.

## Tiefergehende Informationen

Um tiefer in die Überprüfung von Verzeichnissen in Rust einzutauchen, ist es hilfreich zu verstehen, wie das `std::fs::Metadata` Struktur aufgebaut ist. Sie enthält Informationen wie Dateigröße, Änderungszeit, Zugriffsrechte und ob es sich um eine Datei oder ein Verzeichnis handelt.

Zusätzlich zu `std::fs::metadata()` gibt es auch andere Funktionen wie `std::fs::symlink_metadata()` und `std::fs::read_dir()`, die bei der Überprüfung von Dateien und Verzeichnissen nützlich sein können. Es lohnt sich, diese Funktionen zu erkunden, um ein gründlicheres Verständnis von der Verzeichnisprüfung in Rust zu erhalten.

## Siehe auch

- [Rust Standardbibliothek](https://doc.rust-lang.org/std/)
- [Rust Dokumentation über Dateien und Verzeichnisse](https://doc.rust-lang.org/std/fs/index.html)
- [Tutorial: Dateien und Verzeichnisse in Rust](https://stevedonovan.github.io/rust-gentle-intro/6-files.html)