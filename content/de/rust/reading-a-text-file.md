---
title:                "Rust: Eine Textdatei lesen"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist eine grundlegende Fähigkeit, die für viele Programmieraufgaben benötigt wird. In diesem Blogbeitrag werden wir uns ansehen, wie man in Rust eine Textdatei einliest und verarbeitet.

## Wie man eine Textdatei in Rust einliest

Um eine Textdatei in Rust zu lesen, verwenden wir die Standardbibliotheksfunktion `std::fs::read_to_string()`. Diese Funktion nimmt den Pfad zur Datei als Argument und gibt einen `String` mit dem Inhalt der Datei zurück. Hier ist ein einfaches Beispiel, wie man eine Textdatei mit diesem Ansatz einlesen kann:

```Rust
use std::fs;

fn main() {
    let path = "beispiel.txt";
    let text = fs::read_to_string(path).expect("Konnte Datei nicht einlesen");

    println!("{}", text);
}
```

Wenn wir diesen Code ausführen, sollten wir den Inhalt der Datei `beispiel.txt` auf der Konsole ausgegeben bekommen. Natürlich können wir den `String` in `text` auch weiterverarbeiten, zum Beispiel spliten um die Datei Zeile für Zeile zu lesen. In den Links in der "Siehe Auch" Sektion findet ihr weitere Beispiele und Ressourcen, um dies zu tun.

## Eine Ausführliche Erklärung

Jetzt wo wir wissen, wie man eine Textdatei in Rust einliest, lassen Sie uns etwas tiefer in die Details schauen.

Die Funktion `read_to_string()` ist Teil der Standardbibliothek `std::fs`, die verschiedene Funktionen rund um das Lesen und Schreiben von Dateien bereitstellt. Die Funktion `read_to_string()` nimmt den Pfad zur Datei als Argument und gibt einen `Result` zurück. Der `Result` Typ ist ein numerischer Typ, der entweder den Inhalt der Datei (`String`) oder einen Fehler enthält. Im obigen Beispiel haben wir die `expect()` Methode verwendet, um im Falle eines Fehlers eine Fehlermeldung auszugeben. Es ist jedoch ratsam, im Code Vorsichtsmaßnahmen zu treffen, um mit Fehlern umzugehen.

Es ist auch zu beachten, dass die `read_to_string()` Funktion die gesamte Datei auf einmal einliest. Wenn wir große Dateien lesen müssen, kann dies zu Problemen führen. In solchen Fällen ist es besser, die Datei in Chunks zu lesen, um Memory-Probleme zu vermeiden.

## Siehe auch

- [Offizielle Rust Dokumentation: Dateiverarbeitung](https://doc.rust-lang.org/std/fs/index.html)
- [Beispiel von Rust By Example: Dateizugriff](https://rustbyexample.com/std_misc/file/read_lines.html)
- [Stack Overflow: Wie liest man eine Datei Zeile für Zeile in Rust?](https://stackoverflow.com/questions/27394662/how-do-i-read-lines-from-a-file-in-rust)
- [Rust Cookbook: Lesen und Schreiben von Dateien](https://rust-lang-nursery.github.io/rust-cookbook/file/reading.html)