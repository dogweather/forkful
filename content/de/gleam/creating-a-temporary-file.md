---
title:                "Eine temporäre Datei erstellen"
html_title:           "Gleam: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit dem Erstellen von temporären Dateien beschäftigen? In der Programmierung kann es manchmal nützlich sein, temporäre Dateien zu erstellen, um Daten zwischen verschiedenen Prozessen oder Programmen auszutauschen. Durch das Erstellen einer temporären Datei können Daten effizient und sicher übertragen werden, ohne die bestehende Dateistruktur zu verändern.

## Wie

Um in Gleam eine temporäre Datei zu erstellen, können wir die Funktion `File.write_to_temp()` verwenden. Hier ist ein Beispielcode, der eine einfache Textdatei erstellt und diese dann in der temporären Datei speichert.

```Gleam
let content = "Dies ist ein Beispielinhalt."
let file_name = "beispiel.txt"

// Datei erstellen
File.write_to_file(file_name, content)

// Temporäre Datei erstellen
let temp_file = File.write_to_temp(file_name)

// Inhalt der temporären Datei ausgeben
File.read(temp_file.path)
```

Die Ausgabe dieses Codes sollte der Inhalt der erstellten Datei sein, also "Dies ist ein Beispielinhalt." Die temporäre Datei wird automatisch gelöscht, sobald das Programm beendet wird.

## Deep Dive

Die Funktion `File.write_to_temp()` erstellt nicht nur eine temporäre Datei, sondern gibt auch eine `TempFile`-Struktur zurück, die Informationen über die erstellte Datei enthält. Diese Struktur beinhaltet den Namen und den Pfad der temporären Datei sowie Funktionen zum Lesen, Schreiben und Löschen der Datei. Dadurch haben wir mehr Kontrolle über die temporäre Datei und können sie gezielt verwenden.

## Siehe auch

- [Gleam Dokumentation über das Modul File](https://gleam.run/modules/file.html)
- [Beispielprojekt für die Verwendung von temporären Dateien in Gleam](https://github.com/gleam-lang/gleam_by_example/tree/master/tempfile)