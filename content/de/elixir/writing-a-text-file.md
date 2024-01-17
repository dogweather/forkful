---
title:                "Eine Textdatei schreiben"
html_title:           "Elixir: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben einer Textdatei ist ein Weg für Programmierer*innen, um Daten in einer organisierten und lesbaren Form zu speichern. Es ist nützlich, um Informationen oder Ergebnisse aus einer Anwendung oder einem Skript zu sichern und später darauf zugreifen zu können.

## Wie geht's?
Das Schreiben einer Textdatei in Elixir ist einfach. Verwenden Sie die Funktion `File.write/2`, um eine Datei mit den angegebenen Daten zu erstellen oder zu überschreiben. Hier ist ein Beispiel:

```Elixir
File.write("mein_text.txt", "Hallo Welt!")
```
Dieser Code erstellt eine neue Textdatei mit dem Namen "mein_text.txt" und schreibt "Hallo Welt!" in die Datei.

## Tief eintauchen
Das Schreiben von Textdateien hat eine lange Geschichte und wurde zuerst von Programmierern verwendet, um Daten in einer organisierten Form zu speichern. Alternativ kann auch die Funktion `IO.write/2` verwendet werden, die jedoch keine Datei erstellt oder überschreibt, sondern direkt auf die Konsole schreibt. Beim Schreiben von Textdateien müssen auch eventuelle Fehler beim Öffnen oder Schreiben der Datei beachtet werden.

## Siehe auch
Weitere Informationen und Beispiele zum Schreiben von Textdateien in Elixir finden Sie in der offiziellen Dokumentation unter: https://hexdocs.pm/elixir/File.html#write/2