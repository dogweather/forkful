---
title:                "Eine Textdatei lesen"
html_title:           "Gleam: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen von Textdateien ist eine grundlegende Aufgabe für Programmiererinnen und Programmierer. Egal ob du Daten analysieren, Texte verarbeiten oder einfach nur in deinem Code navigieren möchtest, das Lesen von Textdateien ist ein wichtiges Werkzeug.

# Wie geht das?

Um eine Textdatei in Gleam zu lesen, kannst du die `File`-Bibliothek verwenden. Als ersten Schritt musst du die Datei öffnen, dazu verwendest du die Funktion `File.open`. Diese Funktion erwartet zwei Argumente: den Pfad zur Datei und den Modus, in dem die Datei geöffnet werden soll.

Dann kannst du die Datei Zeile für Zeile lesen, indem du die Funktion `File.readline` verwendest. Mit dieser Funktion liest du eine einzelne Zeile aus der Datei und gibst sie als String zurück. Du kannst diese Funktion in einer Schleife verwenden, um die gesamte Datei zu lesen.

```Gleam
let file = File.open("./meine-datei.txt", File.Read)
let line = File.readline(file)
```

Ein Beispielprogramm, das alle Zeilen aus einer Datei liest und auf der Konsole ausgibt, könnte so aussehen:

```Gleam
import gleam/console
import gleam/file

pub fn read_file() {
  let file = File.open("./meine-datei.txt", File.Read)
  loop {
    case File.readline(file) {
      Ok(line) -> console.log(line)
      Error(_error) -> break
    }
  }
}
```

Das könnte zum Beispiel folgende Ausgabe erzeugen:

```
Dies ist die erste Zeile meines Textes
Und das ist Zeile 2
Hier folgt Zeile 3
Ach ja, eine vierte Zeile gibt es auch noch
```

# Tiefer eintauchen

Das Lesen von Textdateien ist in der Regel ein relativ einfacher Vorgang, aber es gibt einige Dinge, die du beachten solltest. Wenn du Textdateien liest, musst du sicherstellen, dass die Datei überhaupt existiert und dass du die richtige Codierung verwendest. Außerdem solltest du darauf achten, die Datei nach dem Lesen wieder ordnungsgemäß zu schließen.

Wenn du tiefer in die Thematik einsteigen möchtest, empfehlen wir dir, die Dokumentation zu der `File`-Bibliothek zu lesen. Diese findest du hier: https://gleam.run/lib/gleamio/file/latest/

# Sieh dir auch diese Links an

- [Gleam Homepage](https://gleam.run/)
- [Gleam Dokumentation auf Deutsch](https://gleam.run/de/docs/)
- [Beispiele für Gleam Code](https://github.com/gleam-lang/gleam/tree/master/examples)