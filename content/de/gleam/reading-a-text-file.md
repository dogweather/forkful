---
title:                "Gleam: Ein Textdokument lesen"
simple_title:         "Ein Textdokument lesen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

##Warum
Warum sollte man sich die Zeit nehmen, einen Text zu lesen? Die Antwort ist einfach: Textdateien sind ein wesentlicher Bestandteil der Programmierung und werden häufig als Datenquelle für Code verwendet. Indem man lernt, wie man Textdateien richtig liest, kann man seine Programmierfähigkeiten verbessern und effektivere Code schreiben.

##How To
Das Lesen einer Textdatei in Gleam ist ein grundlegender Prozess, der jedoch für viele Neu- und Fortgeschrittene Programmierer verwirrend sein kann. Um klarzustellen, wie es gemacht wird, folgen hier einige einfache Code-Beispiele, die zeigen, wie man eine Textdatei in Gleam öffnet, liest und schließt.

```Gleam
fname = "meinTextdokument.txt"

file = open(fname)

content = read(file)

print(content)

close(file)
```
Hier wird zuerst der Dateiname in einer Variablen gespeichert. Dann wird die Datei mit der `open` Funktion geöffnet und ihre Inhalte mit der `read` Funktion in eine Variable geladen. Schließlich wird der Inhalt mit der `print` Funktion ausgegeben und die Datei mit der `close` Funktion geschlossen.

##Deep Dive
Das Lesen einer Textdatei in Gleam gibt dir die volle Kontrolle über ihre Inhalte. Man kann sie in Teile aufteilen, spezifische Zeilen lesen oder sogar die Daten in andere Formate umwandeln. Gleam bietet auch verschiedene Funktionen, um Textdateien effizient zu lesen, z. B. `read_line`, `read_chunk` und `read_file`.

Außerdem gibt es in Gleam Bibliotheken wie `std/filesystem` und `std/file` mit nützlichen Funktionen zum Lesen und Verarbeiten von Textdateien. Während der Textverarbeitung mit Gleam noch komplizierter sein kann als mit anderen Sprachen, bieten diese Bibliotheken viele hilfreiche Werkzeuge, um die Arbeit zu erleichtern.

##Siehe auch
- [Gleam-Dokumentation zur Textverarbeitung](https://gleam.run/documentation/stdlib-filesystem)
- [Artikel zu Dateiverarbeitung mit Gleam](https://dev.to/reverentgeek/reading-and-writing-files-with-gleam-14l9)
- [Tutorial zur Verarbeitung von Textdateien in Gleam](https://edofic.com/posts/reading-files-gleam/)