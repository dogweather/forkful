---
title:                "Eine Textdatei lesen"
html_title:           "Elixir: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Textdateien lesen ist eine gängige Aufgabe für Programmierer. Dabei geht es darum, den Inhalt einer Textdatei in das Programm einzulesen und damit zu arbeiten. Dies kann nützlich sein, wenn man große Datenmengen übersichtlich verarbeiten möchte oder bestimmte Informationen aus einer Textdatei extrahieren muss.

## Wie geht's?

Um eine Textdatei in Elixir zu lesen, verwenden wir die Funktion ```File.read/1```. Diese Funktion erwartet als Argument den Dateinamen der Textdatei und gibt den Inhalt als Zeichenkette zurück. Hier ein Beispiel:

```elixir
content = File.read("beispiel.txt")
IO.puts content
```

In diesem Beispiel wird die Datei "beispiel.txt" eingelesen und der Inhalt in der Variablen "content" gespeichert. Mit der Funktion ```IO.puts/1``` wird der Inhalt dann auf der Konsole ausgegeben.

## Tiefentauchen

Das Lesen von Textdateien ist eine grundlegende Funktion, die in Elixir mit der integrierten Modulbibliothek "File" gelöst wird. Dabei können verschiedene Konvertierungsmöglichkeiten, wie z.B. UTF-8, eingestellt werden. Eine Alternative zur Funktion ```File.read/1``` ist die Funktion ```File.stream!/2```, welche den Inhalt zeilenweise in einem Strom von Daten zurückgibt. Im Hintergrund wird dabei die Funktion ```IO.stream/1``` verwendet. Die Implementierung des Lesens von Textdateien in Elixir basiert auf dem "IO Device"-Protokoll, welches auch für die Ein- und Ausgabe in Dateien verwendet wird. Weitere Informationen zu diesem Protokoll finden sich in der offiziellen Dokumentation von Elixir.

## Siehe auch

- Offizielle Dokumentation von Elixir zu Textdateien: https://hexdocs.pm/elixir/File.html
- ElixirForum Diskussion über das Lesen von Textdateien: https://elixirforum.com/t/reading-lines-from-text-file/10489