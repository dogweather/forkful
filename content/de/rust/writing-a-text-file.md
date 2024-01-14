---
title:    "Rust: Eine Textdatei schreiben"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Rust ist eine aufstrebende Programmiersprache, die durch ihre Performance und Sicherheit immer mehr an Beliebtheit gewinnt. Das Schreiben einer Textdatei kann eine einfache und alltägliche Aufgabe sein, aber auch hier ist es sinnvoll, die Vorteile von Rust zu nutzen. In diesem Beitrag erfährst du, wie du eine Textdatei in Rust schreiben kannst und warum es sich lohnen kann.

## Wie geht das?

Um eine Textdatei in Rust zu schreiben, benötigst du das Modul "std::fs" (Filesystem). Mit diesem Modul kannst du auf das Dateisystem zugreifen und die Datei erstellen oder ändern. Als erstes Beispiel erstellen wir eine neue Datei namens "beispiel.txt" und schreiben den Text "Hallo Welt!" hinein.

```Rust
use std::fs::File; // Imports des File Moduls
use std::io::prelude::*; //Imports des std::io Moduls

fn main() {
    let mut file = File::create("beispiel.txt") // Erstellt eine neue Datei
        .expect("Datei konnte nicht erstellt werden"); // Fehlerbehandlung

    file.write_all(b"Hallo Welt!") // Schreibt den Text "Hallo Welt!" in die Datei, b muss angegeben werden, da es sich um einen "Byte String" handelt
        .expect("Fehler beim Schreiben der Datei"); // Fehlerbehandlung
}
```

Wenn du nun die Datei "beispiel.txt" öffnest, wirst du den Text "Hallo Welt!" darin finden. Natürlich kannst du auch andere Daten oder Variablen in eine Textdatei schreiben, indem du sie als "Byte String" formatierst.

## Tiefer tauchen

Das Erstellen und Schreiben einer Textdatei ist nur eines von vielen Anwendungsbeispielen für das "std::fs" Modul. Du kannst auch auf bereits vorhandene Dateien zugreifen und diese lesen oder ändern. Zum Beispiel könntest du eine Textdatei auslesen und die Daten in eine Variable speichern, um sie weiterzuverarbeiten. Hier ein kurzes Beispiel:

```Rust
use std::fs::File; // Imports des File Moduls
use std::io::prelude::*; // Imports des std::io Moduls

fn main() {
    let mut file = File::open("beispiel.txt") // Öffnet die Datei zum Lesen
        .expect("Datei konnte nicht geöffnet werden"); // Fehlerbehandlung

    let mut data = String::new(); // Erstellt eine neue String Variable, um die Daten zu speichern

    file.read_to_string(&mut data) // Liest die Daten aus der Datei und speichert sie in der Variable
        .expect("Fehler beim Lesen der Datei"); // Fehlerbehandlung

    println!("{}", data); // Gibt die gespeicherten Daten auf der Konsole aus
}
```

In diesem Beispiel nutzen wir die Methode `read_to_string()` um die Daten in einer String Variable zu speichern. Es gibt aber auch andere Methoden, die es dir ermöglichen, die Daten in einem bestimmten Format zu speichern.

## Siehe auch

- [Offizielle Rust Dokumentation zum Modul "std::fs"](https://doc.rust-lang.org/std/fs/index.html)
- [Tutorial: Dateien lesen und schreiben in Rust](https://authenticfive.com/tutorial-lesen-und-schreiben-von-dateien-in-rust/)
- [Vergleich von Rust mit anderen Programmiersprachen](https://www.tiobe.com/tiobe-index/rust/)