---
title:    "Rust: Lesen von Befehlszeilenargumenten"
keywords: ["Rust"]
---

{{< edit_this_page >}}

# Warum

Das Einlesen von Befehlszeilenargumenten ist ein grundlegender Aspekt in der Programmierung, der die Funktionalität des Programms erweitert und es dem Benutzer ermöglicht, Eingaben zu machen. In diesem Beitrag werden wir uns mit der Verwendung von Rust beschäftigen, um Befehlszeilenargumente einzulesen.

# Wie geht man vor?

Um Befehlszeilenargumente in Rust einzulesen, müssen wir zuerst die Standardbibliothek verwenden. Dies kann durch Hinzufügen der Zeile `use std::env;` am Anfang unseres Codes erreicht werden. Dann müssen wir die Funktion `args()` verwenden, um eine Liste der Argumente zu erhalten, die der Benutzer beim Start des Programms eingegeben hat.

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    println!("{:?}", args);
}
```

In diesem Beispiel speichern wir die Argumente in einem Vektor mit dem Namen `args` und geben sie dann mit `println!()` aus. Wenn wir unser Programm nun über die Befehlszeile aufrufen und dabei Argumente übergeben, werden diese in unserem Output angezeigt.

```
$ cargo run argument1 argument2
["target/debug/programm", "argument1", "argument2"]
```

# Tiefer tauchen

Ein tiefergehenderer Aspekt des Einlesens von Befehlszeilenargumenten in Rust ist die Überprüfung der Argumente auf ihre Struktur und Gültigkeit. Dies kann durch Verwendung von Mustern und dem `matches()`-Makro erreicht werden.

```Rust
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let first_arg = &args[0];
    match first_arg {
        "help" => println!("Dieses Programm benötigt Hilfe."),
        "version" => println!("Diese Version ist 1.0."),
        _ => println!("Unbekanntes Argument."),
    }
}
```

In unserem Beispiel überprüfen wir das erste Argument und zeigen dann eine entsprechende Nachricht an. Wenn keine der Bedingungen zutrifft, wird eine Standardnachricht ausgegeben.

# Siehe auch

Weitere Informationen zum Einlesen von Befehlszeilenargumenten in Rust finden Sie in der offiziellen Dokumentation: [https://doc.rust-lang.org/std/env/fn.args.html](https://doc.rust-lang.org/std/env/fn.args.html)

Weitere hilfreiche Ressourcen können auf der offiziellen Rust-Website gefunden werden: [https://www.rust-lang.org/](https://www.rust-lang.org/)

Wir hoffen, dass dieser Beitrag Ihnen geholfen hat, die Grundlagen des Einlesens von Befehlszeilenargumenten in Rust zu verstehen. Viel Spaß beim Programmieren!