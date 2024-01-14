---
title:    "Rust: Lesen einer Textdatei"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Warum

Wenn Sie neu in der Welt der Programmierung sind oder einfach nur ein neues Programmiersprachenlernern möchten, kann Rust eine großartige Wahl sein. Eine häufige Aufgabe in der Softwareentwicklung ist es, Daten aus externen Dateien zu lesen, wie zum Beispiel einer Textdatei. In diesem Beitrag werden wir besprechen, wie man eine Textdatei in Rust effizient lesen kann.

## Wie Gehen Sie Vor

Um Daten aus einer Textdatei in Rust zu lesen, müssen Sie zunächst die entsprechenden Bibliotheken importieren. Wir werden den `std::fs` und den `std::io` Namespace benötigen. Danach können wir die Datei öffnen mit Hilfe der `File::open()` Funktion:

```Rust
use std::fs::File;
use std::io::prelude::*;

let mut file = File::open("textdatei.txt").expect("Konnte die Datei nicht öffnen");
```

Hier haben wir die `textdatei.txt` Datei geöffnet, die sich im selben Verzeichnis wie unser Programm befindet. Achten Sie darauf, dass der Pfad relativ zur Position des Programms angegeben wird. Wenn die Datei nicht vorhanden ist, wird das Programm eine Fehlermeldung ausgeben.

Als nächstes müssen wir die Daten aus der Datei lesen und in einen String speichern. Wir können dies mit der `read_to_string()` Funktion und einer `String` Variable machen:

```Rust
let mut contents = String::new();

// Inhalt aus der Datei in den String lesen
file.read_to_string(&mut contents)
	.expect("Konnte den Inhalt der Datei nicht lesen");
```

Nun enthält die `contents` Variable den gesamten Inhalt der Datei als String. Wir können diesen String nun weiterverarbeiten oder einfach ausgeben:

```Rust
println!("Der Inhalt der Datei ist: {}", contents);
```

## Tiefes Eintauchen

Jetzt, da wir gelernt haben, wie man eine Textdatei in Rust liest, können wir uns ein wenig tiefer in die Details begeben. Zum Beispiel, was passiert, wenn die Datei sehr groß ist und unsere `String` Variable nicht genügend Speicher hat, um den gesamten Inhalt zu speichern? In solchen Fällen können wir die `std::io::BufReader` Klasse verwenden, die Daten in kleinere "Chunks" aufteilt und diese direkt aus der Datei liest, anstatt alles auf einmal zu versuchen. Wir können auch die `read_line()` Funktion verwenden, um Zeilenweise durch die Datei zu iterieren anstatt den gesamten Inhalt auf einmal zu lesen.

Es gibt auch viele andere Optionen und Funktionen, die es ermöglichen, Textdateien in Rust effizienter und flexibler zu lesen. Es lohnt sich, sich damit auseinanderzusetzen, um Ihre Programmierkenntnisse zu verbessern.

## Siehe Auch

- Die offizielle Rust Dokumentation zum Thema "Reading and Writing Files": https://doc.rust-lang.org/book/ch12-00-an-io-project.html#reading-a-file
- Eine Quora Antwort, die verschiedene Methoden und Techniken zum Lesen von Textdateien in Rust behandelt: https://www.quora.com/How-do-I-read-text-files-in-Rust