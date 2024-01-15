---
title:                "Eine Textdatei schreiben"
html_title:           "Gleam: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Schreiben einer Textdatei ist eine grundlegende Fähigkeit, die jeder Programmierer benötigt. Mit Gleam können wir diese Aufgabe effizient und intuitiv erledigen.

## Wie geht das?

Um eine Textdatei mit Gleam zu schreiben, müssen wir zunächst die `File` Bibliothek importieren:

```Gleam
import gleam/io/file
```

Als nächstes erstellen wir eine Datei mit dem gewünschten Namen und Speicherort und öffnen sie im Schreibmodus:
```Gleam
let file = file.open("meine_datei.txt", gleam/io/file.Write)
```

Anschließend können wir Inhalt in die Datei schreiben, indem wir die Funktion `write` aufrufen und den Text als Argument übergeben:
```Gleam
file.write("Dies ist ein Beispieltext für unsere Datei.")
```

Um sicherzustellen, dass der Inhalt in der Datei gespeichert wird, müssen wir den `commit` Befehl ausführen:
```Gleam
file.commit()
```

Wenn wir fertig sind, müssen wir die Datei schließen, um Speicherressourcen freizugeben:
```Gleam
file.close()
```

Die fertige Funktion könnte wie folgt aussehen:
```Gleam
pub fn write_to_file(content: String, file_name: String) {
    let file = file.open(file_name, gleam/io/file.Write)
    file.write(content)
    file.commit()
    file.close()
}
```

## Tiefer eintauchen

Es ist auch möglich, den Inhalt einer Datei durch die Verwendung des `write_line` Befehls Zeile für Zeile zu schreiben. Dies kann nützlich sein, wenn wir eine Liste von Werten oder Objekten in die Datei schreiben möchten. Wir können auch zusätzliche Optionen wie das Hinzufügen von Leerzeichen oder den Zeilenumbruch anpassen.

Weitere Details und Beispiele findest du in der offiziellen Dokumentation der `File` Bibliothek.

## Siehe auch

- Offizielle Dokumentation der `File` Bibliothek: https://gleam.run/modules/gleam_io_file/latest/
- Einführung in Gleam: https://gleam.run/getting-started/
- Weitere Gleam-Artikel: https://gleam.run/articles/