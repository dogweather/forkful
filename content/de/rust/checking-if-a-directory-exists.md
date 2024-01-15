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

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein häufiger Teil des Programmierens. Es ist wichtig, um Fehler zu vermeiden und sicherzustellen, dass der Code wie erwartet funktioniert. In dieser Anleitung werden wir uns ansehen, wie man in Rust überprüft, ob ein Verzeichnis vorhanden ist.

## Wie geht das?

Die Überprüfung, ob ein Verzeichnis existiert, ist in Rust ziemlich einfach. Dazu verwenden wir die Funktion `metadata()` aus dem `std::fs` Modul. Schauen wir uns ein Beispiel an:

```Rust
use std::fs;

fn main() {
    let directory = "Pictures"; // Verzeichnis, das überprüft werden soll

    // Überprüfen, ob das Verzeichnis existiert
    if let Ok(metadata) = fs::metadata(directory) {
        // Wenn das Verzeichnis existiert, Ausgabe in der Konsole
        println!("Das Verzeichnis {} existiert.", directory);
    } else {
        // Wenn das Verzeichnis nicht existiert, Ausgabe in der Konsole
        println!("Das Verzeichnis {} existiert nicht.", directory);
    }
}
```

Die Ausgabe dieses Codes sollte sein:

```
Das Verzeichnis Pictures existiert.
```

In diesem Beispiel verwenden wir die `if let` Syntax, um die `metadata()` Funktion auszuführen und das Ergebnis zu überprüfen. Wenn das Verzeichnis existiert, wird keine Fehlermeldung ausgegeben und es wird bestätigt, dass das Verzeichnis vorhanden ist.

## Tiefere Einblicke

Wenn du tiefer in das Überprüfen von Verzeichnissen in Rust eintauchen möchtest, gibt es noch ein paar Dinge zu beachten. Zum Beispiel gibt die `metadata()` Funktion ein `Result` Objekt zurück, das entweder ein `Ok` Ergebnis (wenn das Verzeichnis existiert) oder ein `Err` Ergebnis (wenn das Verzeichnis nicht existiert oder ein Fehler aufgetreten ist) sein kann.

Um zu kontrollieren, ob das Verzeichnis vorhanden ist, könnten wir auch `unwrap()` verwenden, anstatt die `if let` Syntax zu verwenden. Dies würde dazu führen, dass das Programm abbricht, wenn ein Fehler auftritt. Eine weitere Option ist `expect()`, wo wir eine benutzerdefinierte Fehlermeldung angeben können, die ausgegeben wird, wenn ein Fehler auftritt.

Schließlich gibt es auch die Funktion `canonicalize()`, die den Pfad eines Verzeichnisses bereinigt und normalisiert. Dies ist besonders nützlich, wenn du sicherstellen möchtest, dass der angegebene Pfad so ist, wie es für das Programm erwartet wird.

## Siehe auch

- [Rust Dokumentation zu Verzeichnissen](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [Rust Dokumentation zu `Result`](https://doc.rust-lang.org/std/result/enum.Result.html)
- [Rust Dokumentation zu `expect()` und `unwrap()`](https://doc.rust-lang.org/std/result/enum.Result.html#method.expect)