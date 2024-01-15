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

#Warum

Es gibt viele Gründe, warum man einen Text lesen möchte. Vielleicht möchtest du eine Datei analysieren oder Informationen aus einer Textdatei in dein Programm laden. Was auch immer der Grund sein mag, in diesem Artikel zeige ich dir, wie du mit Rust eine Textdatei einlesen kannst.

##Wie geht das?

Es ist wirklich einfach, eine Textdatei mit Rust zu lesen. Zuerst müssen wir die `std::fs` Bibliothek importieren, um auf Dateien zugreifen zu können. Dann verwenden wir die `std::fs::File` Funktion, um eine Datei zu öffnen. Anschließend können wir die `.read_to_string()` Methode verwenden, um den Inhalt der Datei in einen String zu lesen.

```Rust
use std::fs::File;

let mut file = File::open("text.txt").expect("Datei nicht gefunden");
let mut content = String::new();
file.read_to_string(&mut content).expect("Konnte Datei nicht lesen");
println!("{}", content);
```

Im obigen Beispiel haben wir eine Textdatei mit dem Namen "text.txt" geöffnet und ihren Inhalt in einen String namens `content` eingelesen. Wir haben auch Error-Handling hinzugefügt, falls die Datei nicht gefunden werden kann oder nicht gelesen werden kann. Schließlich geben wir den Inhalt der Datei auf der Konsole aus.

##In die Tiefe

Es gibt auch alternative Methoden, um eine Textdatei in Rust zu lesen. Anstatt den Inhalt in einen String zu lesen, können wir auch einen Vektor von Bytes mit der `.read_to_end()` Methode erhalten. Diese Methode gibt ein `Result` Objekt zurück, das einen Vektor von Bytes enthält.

```Rust
let mut file = File::open("text.txt").expect("Datei nicht gefunden");
let mut content = Vec::new();
file.read_to_end(&mut content).expect("Konnte Datei nicht lesen");
println!("{:?}", content);
```

Außerdem gibt es noch die `.lines()` Methode, die es uns ermöglicht, den Inhalt der Datei zeilenweise zu lesen. Diese Methode gibt einen `Lines` Iterator zurück, den wir dann durchlaufen und die einzelnen Zeilen ausgeben können.

```Rust
let mut file = File::open("text.txt").expect("Datei nicht gefunden");
let lines = BufReader::new(&file).lines();

for line in lines {
    println!("{}", line.unwrap());
}
```

#Siehe auch

- Rust's `std::fs` Dokumentation: https://doc.rust-lang.org/std/fs/index.html
- Rust's `std::fs::File` Dokumentation: https://doc.rust-lang.org/std/fs/struct.File.html
- "How to Read a File in Rust" von The Rust Programming Language Blog: https://blog.rust-lang.org/2015/04/10/Iterators.html