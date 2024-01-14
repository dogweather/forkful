---
title:    "Rust: Lesen von Befehlszeilenargumenten"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

Wenn du ein Python-Programmierer bist, kennst du wahrscheinlich die `sys.argv`-Liste zum Lesen von Befehlszeilenargumenten. Aber wusstest du, dass Rust auch eine integrierte `std::env`-Bibliothek hat, die das Lesen von Befehlszeilenargumenten noch einfacher macht? In diesem Blogbeitrag werden wir uns ansehen, wie man Befehlszeilenargumente in Rust liest und warum es eine nützliche Fähigkeit ist.

# Wie man Befehlszeilenargumente in Rust liest

Um Befehlszeilenargumente in Rust zu lesen, müssen wir die `std::env`-Bibliothek importieren. Dann können wir die Funktion `args()` aufrufen, um eine Sammlung von Strings mit den übergebenen Argumenten zu erhalten. Schauen wir uns ein Beispiel an:

```Rust
use std::env;

fn main() {
    // Lesen der Befehlszeilenargumente
    let args: Vec<String> = env::args().collect();

    // Ausgabe der Befehlszeilenargumente
    for arg in args {
        println!("Argument: {}", arg);
    }
}
```

Wenn du dieses Programm mit dem Befehl `cargo run test1 test2` ausführst, sollte die Ausgabe folgendermaßen aussehen:

```
Argument: target/debug/program_name
Argument: test1
Argument: test2
```

Wie du sehen kannst, gibt uns die `args()`-Funktion eine Sammlung von Strings mit den übergebenen Argumenten. Beachte, dass der erste String `target/debug/program_name` ist, was der Pfad zum ausführbaren Programm ist. Wenn du diese Ausgabe nicht haben möchtest, kannst du die `args()`-Funktion auch mit `skip(1)` kombinieren, um den ersten String (den Pfad zum ausführbaren Programm) zu überspringen.

# Tiefer Einblick

In der `std::env`-Bibliothek gibt es noch einige weitere nützliche Funktionen zum Lesen von Befehlszeilenargumenten. Hier sind einige der wichtigsten:

- `current_dir()`: Liefert den Pfad zum aktuellen Arbeitsverzeichnis als `PathBuf`-Objekt zurück.
- `home_dir()`: Liefert den Pfad zum Home-Verzeichnis des Benutzers als `Option<PathBuf>` zurück.
- `var()`: Liefert den Wert einer Umgebungsvariablen als `Result<String>` zurück.
- `var_os()`: Liefert den Wert einer Umgebungsvariablen als `Option<OsString>` zurück.

Du kannst mehr über diese Funktionen und andere Möglichkeiten, Befehlszeilenargumente in Rust zu lesen, in der offiziellen Dokumentation der `std::env`-Bibliothek erfahren.

# Siehe auch

- [Rust-Dokumentation zu `std::env`](https://doc.rust-lang.org/std/env/)
- [Ein anderes Beispiel zum Lesen von Befehlszeilenargumenten in Rust](https://stevedonovan.github.io/rust-gentle-intro/1-basics.html#command-line-arguments)