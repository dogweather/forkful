---
title:                "Rust: Prüfen, ob ein Verzeichnis existiert"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

In der Programmierung gibt es viele Situationen, in denen es wichtig ist zu überprüfen, ob ein bestimmter Ordner existiert. Das kann zum Beispiel beim Speichern von Dateien oder beim Zugriff auf externe Verzeichnisse notwendig sein. In diesem Blogbeitrag lernst du, wie du mithilfe von Rust schnell und einfach herausfinden kannst, ob ein Ordner existiert oder nicht.

## Anleitung

Um in Rust zu überprüfen, ob ein bestimmter Ordner vorhanden ist, kannst du die Funktion `Path::exists()` aus der Standardbibliothek verwenden. Diese Funktion gibt einen `bool`-Wert zurück, der angibt, ob das angegebene Verzeichnis existiert oder nicht.

```Rust
// Importieren der Standardbibliothek
use std::path::Path;

fn main() {
    // Definieren des Ordnerpfades
    let path = Path::new("/pfad/zum/ordner");

    // Überprüfen, ob der Ordner existiert
    if path.exists() {
        println!("Der Ordner existiert!");
    } else {
        println!("Der Ordner existiert nicht!");
    }
}
```

Wenn der Ordner existiert, gibt das Programm den Text "Der Ordner existiert!" aus, ansonsten wird "Der Ordner existiert nicht!" angezeigt. Das Beispiel oben verwendet einen absoluten Pfad, du kannst aber auch einen relativen Pfad verwenden, um den Ordner im aktuellen Arbeitsverzeichnis zu überprüfen.

## Tiefgreifende Informationen

Wenn du noch tiefer in das Thema einsteigen möchtest, kannst du dich mit der Dokumentation der Standardbibliothek beschäftigen. Dort findest du weitere nützliche Funktionen und Möglichkeiten, um mit Verzeichnissen und Dateipfaden in Rust umzugehen.

Eine wichtige Funktion ist zum Beispiel `is_dir()`, die überprüft, ob es sich bei dem Pfad um ein Verzeichnis handelt. Du kannst auch die Funktion `create_dir()` verwenden, um einen neuen Ordner zu erstellen, falls dieser noch nicht existiert.

# Siehe auch

- [Dokumentation der Standardbibliothek](https://doc.rust-lang.org/std/path/index.html)
- [Weitere nützliche Funktionen für die Arbeit mit Verzeichnissen in Rust](https://doc.rust-lang.org/std/fs/index.html)