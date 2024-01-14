---
title:                "Gleam: Ein Textdokument lesen"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist eine wichtige Fähigkeit in der Programmierung, die Ihnen ermöglicht, Daten aus externen Dateien in Ihr Programm zu integrieren. Dies kann hilfreich sein, um große Datenmengen zu verarbeiten oder um Benutzereingaben zu speichern.

## Wie geht das?

Die Sprache Gleam bietet eine einfache und effiziente Möglichkeit, Textdateien zu lesen. Im Folgenden finden Sie Beispiele für das Lesen von Textdateien mit verschiedenen Methoden.

```Gleam
// Datei öffnen
file := File.open("meine_datei.txt", {|file| file})

// Eine Zeile lesen
gleam_line := File.read_line(file)

// Die Datei Zeile für Zeile lesen und in einer Liste speichern
lines := file
|> File.read_lines
|> List.map(String.trim)
```

Das Ergebnis des obigen Codes ist eine Liste von Strings, die jede Zeile der Textdatei enthält. Durch die Verwendung von `List.map` können Manipulationen an jeder Zeile durchgeführt werden, bevor sie in der Liste gespeichert wird.

## Deep Dive

Wenn Sie tiefer in das Lesen von Textdateien mit Gleam eintauchen möchten, können Sie die Dokumentation von Gleam zu den `File`- und `String`-Modulen konsultieren. Diese bieten eine Vielzahl von Funktionen, die es Ihnen ermöglichen, komplexe Operationen auf Textdateien durchzuführen. Es ist auch wichtig, sich bewusst zu sein, wie Gleam mit Datei- und Zeichenkodierungen umgeht, um unerwartete Ergebnisse zu vermeiden.

## Siehe auch

- [Gleam-Dokumentation zu File-Modul](https://gleam.run/modules/gleam_io/file.html)
- [Gleam-Dokumentation zu String-Modul](https://gleam.run/modules/gleam_core/string.html)
- [Beispielprojekt zum Lesen von Textdateien mit Gleam](https://github.com/gleam-lang/gleam_io_examples)