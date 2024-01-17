---
title:                "Eine Textdatei lesen."
html_title:           "Gleam: Eine Textdatei lesen."
simple_title:         "Eine Textdatei lesen."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen von Textdateien ist eine häufige Aufgabe für Programmierer, bei der sie die Daten in einer Datei auslesen und in ihr Programm einbinden müssen. Dies ist besonders nützlich, wenn es darum geht, große Mengen an Daten in einem strukturierten Format zu speichern und zu verarbeiten.

## Wie geht's?
Um eine Textdatei in Gleam zu lesen, verwenden wir die Funktion `File.read(filename)`, wobei `filename` der Name der Datei ist, die wir lesen möchten. Hier ist ein Beispiel:

```Gleam
let content = File.read("data.txt")
```
Dieser Code liest den Inhalt der Datei "data.txt" und speichert ihn in der Variablen `content`. Es ist wichtig zu beachten, dass der Dateiname den richtigen Pfad zu der Datei enthalten muss, wenn sie nicht im gleichen Ordner wie das Programm liegt.

Ein weiteres nützliches Werkzeug beim Lesen von Textdateien ist die Funktion `String.trim(string)`, die es uns ermöglicht, Leerzeichen und Zeilenumbrüche am Anfang und Ende eines Strings zu entfernen. Dies ist besonders hilfreich, um unerwünschte Leerzeichen zu vermeiden, wenn wir den Inhalt der Datei weiterverarbeiten möchten. Hier ist ein Beispiel:

```Gleam
let trimmed = String.trim(content)
```

## Tiefer gehend
Das Lesen von Textdateien ist ein grundlegender Bestandteil vieler Programmierprojekte, da es uns ermöglicht, externe Daten in unsere Anwendungen zu integrieren. Es ist auch eine wichtige Fähigkeit für angehende Programmierer, die lernen, wie sie mit verschiedenen Dateiformaten umgehen können.

Eine Alternative zum Lesen von Textdateien in Gleam ist die Verwendung von `File.read_newline(filename)`, die den Inhalt der Datei zeilenweise ausliest. Dies kann nützlich sein, wenn wir nur an bestimmten Teilen der Datei interessiert sind.

Bei der Implementierung des Lesens von Textdateien in Gleam ist es wichtig, darauf zu achten, dass die Datei geschlossen wird, nachdem wir sie gelesen haben, um Speicherlecks zu vermeiden. Dafür können wir die Funktion `File.close(file)` verwenden.

## Siehe auch
- [Gleam-Dokumentation über das Lesen von Dateien](https://gleam.run/core/file.html#read)
- [Erlang: Lesen von Dateien](https://erlang.org/doc/man/file.html#read-1)