---
title:                "Befehlszeilenargumente lesen"
html_title:           "Arduino: Befehlszeilenargumente lesen"
simple_title:         "Befehlszeilenargumente lesen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Was & Warum?

Beim Einlesen von Befehlszeilenargumenten nimmt ein Programm zusätzliche Parameter aus der Befehlszeile auf. Dies ist definitiv entscheidend, um Ihr Programm reaktionsschneller und vielseitiger zu machen, indem es ermöglicht, das Verhalten des Programms mit verschiedenen Eingaben zu steuern.

## So geht's:

Mit Rust können wir recht einfach Befehlszeilenargumente einlesen. Hier ein Beispiel:

```Rust
use std::env;

fn main() {
  let args: Vec<String> = env::args().collect();
  println!("{:?}", args);
}
```

Wenn Sie das Programm mit `rustc main.rs && ./main Hallo Welt` ausführen, erhalten Sie:

```Rust
["./main", "Hallo", "Welt"]
```

Sie sehen die Liste aller Argumente, einschließlich des Namens des Programms.

## Tiefere Einblicke

Historisch gesehen ist das Einlesen von Befehlszeilenargumenten ein bewährtes Konzept in der Programmierung, das von frühen DEC-Computersystemen bis hin zu modernen Betriebssystemen reicht. Innerhalb von Rust ermöglicht das `std::env::args()` eine hohe Übersichtlichkeit und Funktionssicherheit. 

Alternativ können Sie die `getopts`- oder die `clap`-Bibliotheken verwenden, welche erweiterte Funktionen zum Parsen von Befehlszeilenargumenten bieten. Sie bieten Konfigurationsoptionen zum Umgang mit Fehlereingaben und zur Erzeugung spannender CLI-Benutzeroberflächen.

Die Implementierungsdetails zur Verwendung dieser Funktionen in Rust sind klar und direkt - sammeln Sie die Befehlszeilenargumente in einem geeigneten Datenstrukturnamens `Vec<String>` zur leichteren Manipulation und Verwendung in Ihrem Programm.

## Siehe auch

Hier weitere Ressourcen:

1. Rust-Dokumentation zu `std::env::args()`: https://doc.rust-lang.org/std/env/fn.args.html
2. Rust-Bibliothek `getopts`: https://docs.rs/getopts/0.2.21/getopts/
3. Rust-Bibliothek `clap`: https://clap.rs/

Die Auswahl der besten Option hängt von den spezifischen Anforderungen Ihres Programms ab. Der Charme von Rust liegt in dieser Flexibilität - es gibt immer eine Methode, die genau das tut, was Sie brauchen.