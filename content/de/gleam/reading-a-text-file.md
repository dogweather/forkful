---
title:    "Gleam: Einen Textdatei lesen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

##Warum
Das Lesen von Textdateien ist eine grundlegende Fähigkeit, die für die effektive Programmierung in Gleam unerlässlich ist. Durch das Lesen von Textdateien kann man Daten aus externen Quellen in das Programm einbinden und weiterverarbeiten.

##Anleitung
Um eine Textdatei in Gleam zu lesen, muss man zunächst die Datei im Programm importieren. Dies kann mit dem Befehl `use` geschehen. Anschließend kann man die Funktion `File.read` verwenden, um den Inhalt der Datei auszulesen. Der Inhalt wird dann in Gleams Standard-Datentyp für Text, `String`, gespeichert.

Beispielcode:

```Gleam
use gleam/io

file_content = File.read("textdatei.txt")

Io.print(file_content)
```

Mögliche Ausgabe:

```
Dies ist der Inhalt der Textdatei!
```

##Vertiefung
Beim Lesen von Textdateien gibt es einige wichtige Dinge zu beachten. Zum einen sollte man immer angeben, in welchem Modus die Datei geöffnet werden soll, also ob sie nur gelesen oder auch beschrieben werden soll. Zum anderen sollte man darauf achten, dass die Datei auch im selben Ordner wie das Programm liegt, ansonsten muss man den Pfad zur Datei angeben.

Außerdem gibt es in Gleam noch weitere nützliche Funktionen, um Textdateien zu lesen, z.B. `File.read_lines`, um den Inhalt als eine Liste von Zeilen auszulesen, oder `File.read_binary`, um den Inhalt als binäre Daten zu lesen.

##Siehe auch
- [Vollständige Gleam Dokumentation](https://gleam.run/documentation/)
- [Einführung in die Gleam Programmierung](https://dev.to/kofi/getting-started-with-gleam-3139)
- [Gleam Community Forum](https://elixirforum.com/c/gleam/14)