---
title:                "Rust: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Warum

In der Programmierung gibt es oft Situationen, in denen es wichtig ist, zu überprüfen, ob ein bestimmtes Verzeichnis existiert. Dies kann hilfreich sein, um Fehler zu vermeiden oder bestimmte Funktionen gezielt anzuwenden. In diesem Blog-Beitrag werden wir uns ansehen, wie man in Rust überprüfen kann, ob ein Verzeichnis existiert, und welchen Nutzen dies haben kann.

# Wie man überprüft, ob ein Verzeichnis existiert

Die einfachste Möglichkeit, um zu überprüfen, ob ein Verzeichnis existiert, ist die Verwendung der `Path`-Struktur aus dem `std::fs` Modul. Diese Struktur bietet eine Funktion namens `exists`, mit der wir überprüfen können, ob ein bestimmtes Verzeichnis oder eine Datei existiert. Sehen wir uns dazu ein Beispiel an:

```Rust
fn main() {
    use std::path::Path;
    
    let directory = Path::new("/home/user/documents");
    if directory.exists() {
        println!("Das Verzeichnis existiert!");
    } else {
        println!("Das Verzeichnis existiert nicht!");
    }
}
```

In diesem Beispiel erstellen wir mit `Path::new` ein `Path`-Objekt für das Verzeichnis "/home/user/documents". Anschließend überprüfen wir mit der `exists`-Funktion, ob dieses Verzeichnis existiert. Je nachdem, ob das Verzeichnis existiert oder nicht, geben wir eine entsprechende Meldung aus.

# Tiefergehende Informationen

Wenn wir uns den Quellcode von Rust genauer ansehen, können wir erkennen, dass die `exists`-Funktion intern die `metadata`-Funktion aufruft. Diese Funktion gibt ein `Metadata`-Objekt zurück, welches Informationen über die Datei oder das Verzeichnis enthält, wie z.B. die Größe oder das Änderungsdatum. Durch die Verwendung der `Metadata`-Struktur können wir also auch noch weitere Informationen über das Verzeichnis abrufen.

# Siehe auch

- [Dokumentation zu std::path::Path](https://doc.rust-lang.org/std/path/struct.Path.html)
- [Dokumentation zu std::fs](https://doc.rust-lang.org/std/fs/index.html)
- [Tutorial zu Rust von The Rust Programming Language](https://doc.rust-lang.org/book/)