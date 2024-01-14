---
title:    "Rust: Überprüfen, ob ein Verzeichnis vorhanden ist."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

In diesem Blog-Beitrag werden wir uns damit beschäftigen, wie man in Rust prüft, ob ein Verzeichnis existiert. Es ist wichtig zu wissen, wie man dies in Rust macht, da es eine häufige Aufgabe ist, wenn man mit Dateien und Verzeichnissen arbeitet. Außerdem ist es ein guter Weg, um das Konzept der Fehlerbehandlung in Rust zu verstehen.

# Wie  man überprüft, ob ein Verzeichnis existiert

Es gibt verschiedene Möglichkeiten, um in Rust zu überprüfen, ob ein Verzeichnis existiert. Hier werden wir zwei der häufigsten Optionen vorstellen.

## Option 1: Mit der `Path` Bibliothek

Die einfachste Möglichkeit, um zu überprüfen, ob ein Verzeichnis existiert, ist die Verwendung der `Path` Bibliothek von Rust. Diese Bibliothek bietet einige nützliche Methoden, um mit Pfaden und Verzeichnissen umzugehen.

Um dies zu nutzen, müssen wir zuerst die Bibliothek im entsprechenden Crate importieren:

```
use std::path::Path;
```

Anschließend können wir die Methode `exists()` von `Path` verwenden, um zu prüfen, ob das Verzeichnis existiert. Hier ist ein Beispielcode, der dies zeigt:

```
let dir_path = Path::new("/home/me/documents");
if dir_path.exists() {
    println!("Das Verzeichnis existiert.");
} else {
    panic!("Das Verzeichnis existiert nicht.");
}
```

## Option 2: Mit der `std::fs` Bibliothek

Eine weitere Möglichkeit, um zu überprüfen, ob ein Verzeichnis existiert, ist die Verwendung der `std::fs` Bibliothek. Diese Bibliothek bietet Funktionen für die Dateisystemoperationen.

Um dies zu nutzen, müssen wir ebenfalls zuerst die Bibliothek im Crate importieren:

```
use std::fs;
```

Anschließend können wir die Funktion `metadata()` verwenden, um Informationen über eine Datei oder ein Verzeichnis abzurufen. Der Rückgabewert ist ein `Result` Type, der Details über die Datei bzw. das Verzeichnis enthält. Wir können dann überprüfen, ob es sich um ein Verzeichnis handelt, indem wir `is_dir()` aufrufen. Hier ist ein Beispielcode:

```
let dir_path = "/home/me/documents";
let metadata = match fs::metadata(dir_path) {
    Ok(metadata) => metadata,
    Err(_) => panic!("Das Verzeichnis existiert nicht.")
};
if metadata.is_dir() {
    println!("Das Verzeichnis existiert.");
} else {
    panic!("Das Verzeichnis existiert nicht.");
}
```

# Tieferer Einblick

Wenn du tiefer in die Details einsteigen möchtest, wie Rust überprüft, ob ein Verzeichnis existiert, empfehlen wir dir, dich mit den verschiedenen Datentypen und Funktionen auseinanderzusetzen, die in der `std::path` und `std::fs` Bibliothek verwendet werden. Diese Artikel können dir dabei helfen:
- [Das `Path` Struct in Rust](https://doc.rust-lang.org/std/path/struct.Path.html)
- [Die `std::fs` Bibliothek in Rust](https://doc.rust-lang.org/std/fs/index.html)

# Siehe auch

Hier sind einige nützliche Ressourcen, die dir helfen können, mehr über die Arbeit mit Dateien und Verzeichnissen in Rust zu erfahren:
- [Die offizielle Rust Dokumentation](https://doc.rust-lang.org/std/fs/index.html)
- [Das Rust Buch - Kapitel über Dateizugriffe](https://doc.rust-lang.org/book/ch12-01-accepting-command-line-arguments.html)
- [Eine Einführung in die Datei- und Verzeichnisoperationen in Rust](https://www.ncameron.org/blog/rust-file-and-directory-operations/)