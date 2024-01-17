---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Rust: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

Was & Warum?
Lesen von Befehlszeilenargumenten ist das Verarbeiten von Eingaben, die ein Programm beim Start von der Befehlszeile erhalten hat. Programmierer tun dies, um interaktive Befehlszeilen-Tools zu erstellen oder um Programme mit benutzerdefinierten Einstellungen auszuführen.

Wie geht's?
In Rust können Befehlszeilenargumente einfach mit der Funktion ```std::env::args()``` gelesen werden. Hier ist ein Beispiel, das das erste Argument (üblicherweise der Dateiname des Programms) und das zweite Argument als ganzzahlige Zahl interpretiert:

```Rust
let args: Vec<String> = std::env::args().collect(); // Sammelt Befehlszeilenargumente in einem Vektor von Strings
let first_arg = &args[1]; // Erstes Argument im Vektor
let second_arg: usize = args[2].parse().unwrap(); // Zweites Argument interpretiert als usize
println!("Arguments: {}, {}", first_arg, second_arg); // Ausgabe: Arguments: Dateiname, Zahl
```

Tiefer tauchen
Das Lesen von Befehlszeilenargumenten ist ein grundlegender Bestandteil der Programmierung, der es ermöglicht, Benutzereingaben auf verschiedene Arten zu verarbeiten. Es gibt alternative Ansätze, wie z.B. die Verwendung von Bibliotheken oder das Parsen von Strings, aber das Lesen von Befehlszeilenargumenten ist die direkteste Methode, um auf diese Eingaben zuzugreifen.

Sieh' auch
Weitere Informationen über die Verwendung von Befehlszeilenargumenten in Rust findest du in der offiziellen Dokumentation: [https://doc.rust-lang.org/std/env/index.html]